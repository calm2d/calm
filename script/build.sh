install_quicklisp () {
    # test Quicklisp
    sbcl --noinform  --disable-debugger --eval "(format t \"~A\" (find-package 'quicklisp-client))" --eval "(quit)" | grep QUICKLISP

    if [ $? -eq 0 ]; then
        echo "Quicklisp already installed."
    else
        echo "Install Quicklisp ..."
        curl -O https://beta.quicklisp.org/quicklisp.lisp

        sbcl --disable-debugger \
             --load ./quicklisp.lisp \
             --eval "(quicklisp-quickstart:install)" \
             --eval "(quit)"

        echo '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' > ~/.sbclrc
        rm ./quicklisp.lisp
    fi
}

dump_calm_bin () {
    echo "Dumping the CALM binary ..."
    sbcl --disable-debugger \
         --load ../../src/calm.asd \
         --eval "(ql:quickload 'calm)" \
         --eval "(asdf:make :calm)"
}

copy_resource () {
    # collect the lisp files
    cp ../../src/core.lisp .
    # copy default gallery
    cp -r ../../gallery .
}

test_calm_bin () {
    # collect the binary
    mv ../../src/calm ./calm-bin

    echo "Test the CALM binary ..."
    CI=True ./calm-bin

    if [ $? -eq 0 ]; then
        echo "SUCCESS!"
    else
        echo "FAIL!"
        exit 42
    fi
}

# https://stackoverflow.com/questions/394230/how-to-detect-the-os-from-a-bash-script
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # ...
    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    echo "linux-$DISTRO"

    echo "Prepare & Switch to build folder ..."
    rm -rf ./build/calm/
    mkdir -p ./build/calm/
    cd ./build/calm/

    echo "Installing dependencies ..."
    if [[ "$DISTRO" == "Ubuntu"* ]]; then
        sudo apt install git libsdl2-2.0-0 libsdl2-mixer-2.0-0  libcairo2 -y
    elif [[ "$DISTRO" == "Fedora"* ]]; then
        sudo dnf install git SDL2 SDL2_mixer cairo -y
    else
        echo "Unsupported DISTRO. Please install dependencies by yourself and modify this script."
        exit 42
    fi


    echo "Copy dependencies ..."
    cp /usr/lib64/libSDL2*.so* .
    cp /usr/lib64/libcairo*.so* .

    # copy all the DLLs required by SDL2 & cairo
    ldd ./*.so* | grep '=> /lib64' | awk '{print $3}' | sort | uniq | xargs -I _ cp _ .
    rm libc.so*

    ls -lah ./

    install_quicklisp

    LD_LIBRARY_PATH=./ dump_calm_bin

    copy_resource

    # collect the launcher
    cp ../../script/calm.sh calm

    test_calm_bin

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    if ! command -v brew &> /dev/null
    then
        echo "Homebrew could not be found, Installing Homebrew ..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    echo "Installing dependencies ..."
    brew install git coreutils sdl2 sdl2_mixer cairo

    # link them, in case of they were unlinked before
    brew link sdl2 sdl2_mixer cairo


    echo "Prepare & Switch to build folder ..."
    rm -rf ./build/calm/
    mkdir -p ./build/calm/
    cd ./build/calm/

    echo "Copy dependencies ..."
    cp /usr/local/lib/libSDL2*.dylib .
    cp /usr/local/lib/libcairo*.dylib .

    echo "Copy every dependency's dependency's dependency's ..."
    # copy all dependencies (`gcp` stands for `GNU cp` in GNU coreutils)
    # this should be ran for many times until no more dylib need to be copied
    # loop 42 times to make sure every dependency's dependency's dependency's ... dependencies are all copied
    for i in {1..42}
    do
        otool -L *.dylib | grep /usr/local | awk '{print $1}' | xargs -I _ gcp -u _ .
    done

    chmod +w *.dylib

    echo "Configure dep tree ..."

    # set LC_RPATH
    # make all of them able to load dependencies from the @loader_path
    for f in *.dylib; do install_name_tool -add_rpath @loader_path/. $f; done

    # change LC_ID_DYLIB
    for f in *.dylib; do install_name_tool -id @rpath/`basename $f` $f; done

    # change LC_LOAD_DYLIB
    # make all of them load dependencies from the @rpath
    for f in *.dylib
    do
        for p in $(otool -L $f | grep /usr/local | awk '{print $1}')
        do
            install_name_tool -change $p @rpath/`basename $p` $f
        done
    done

    # fix libSDL2.dylib
    rm libSDL2.dylib
    ln -s `find libSDL2-*.dylib` libSDL2.dylib

    echo "Unlink the dependencies ..."
    brew unlink sdl2 sdl2_mixer cairo

    ls -lah ./

    install_quicklisp

    dump_calm_bin

    copy_resource

    # collect the launcher
    cp ../../script/calm.sh calm

    test_calm_bin

    echo "Relink the dependencies ..."
    brew link sdl2 sdl2_mixer cairo

    echo "DONE."
elif [[ "$OSTYPE" == "cygwin" ]]; then
    # POSIX compatibility layer and Linux environment emulation for Windows
    echo "Cygwin. Please use MSYS2"
elif [[ "$OSTYPE" == "msys" ]]; then
    # Windows / MSYS2
    echo "Installing dependencies ..."
    pacman -S --noconfirm --needed git \
           mingw64/mingw-w64-x86_64-gcc \
           mingw64/mingw-w64-x86_64-pkgconf \
           mingw64/mingw-w64-x86_64-SDL2 \
           mingw64/mingw-w64-x86_64-SDL2_mixer \
           mingw64/mingw-w64-x86_64-cairo \

    echo "Prepare & Switch to build folder ..."
    rm -rf ./build/calm/
    mkdir -p ./build/calm/
    cd ./build/calm/

    echo "Copy dependencies ..."
    # copy SDL*.dll
    cp /d/msys64/mingw64/bin/SDL*.dll .
    cp /d/msys64/mingw64/bin/libcairo*.dll .
    # copy all the DLLs required by SDL*.dll
    ldd *.dll  | grep mingw | awk '{print $3}' | xargs -I _ cp _ .

    ls -lah ./

    ls -lah "c:\program files\steel bank common lisp\sbcl.exe"

    ls -lah "/c/program files/steel bank common lisp/"

    export PATH="/c/program files/steel bank common lisp/:$PATH"

    install_quicklisp

    dump_calm_bin

    copy_resource

    test_calm_bin

    # windows don't need the bash launcher
    mv ./calm-bin ./calm

elif [[ "$OSTYPE" == "win32" ]]; then
    # I'm not sure this can happen.
    echo "WIN32. Please use MSYS2."
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    # ...
    echo "FREEBSD. Not supported yet"
else
    # Unknown.
    echo "???. Unknown OS. Not supported"
fi
