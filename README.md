# dynmod Installation Instructions
Note: `This style` marks commands to copy-paste into a command line or
configuration file (e.g., .zshrc file on macOS). _This style_ will be specific 
to your username and device.

## Editing Software
Download software for editing model (.dmms) and experiment (.vex) files. We
recommend an editor that can accommodate a custom-made language model for .dmms
and .vex files. 

1. MacOS: We recommend **BBEdit**
([download here](https://www.barebones.com/products/bbedit/); Free Mode is 
sufficient). 
2. To set up BBEdit, download the following files from the Ravasz-Regan Group
GitHub [Repository](https://github.com/Ravasz-Regan-Group/dynmod)
    - Dynamically Modular Model Specification.plist
    - Virtual Experiment.plist
        - Move these files to
        `~/Library/Application\ Support/BBEdit/Language Modules`
3. Windows: We recommend NotePad++
[download here](https://notepad-plus-plus.org).
4. To set up NotePad++, download the following file the Ravasz-Regan Group
GitHub [Repository](https://github.com/Ravasz-Regan-Group/dynmod) and import it
under the Language/User Defined Language tab.
    - DMMS_VEX_Language_Notepad++.xml

## macOS
1. Set up **Terminal** open Terminal and check that it is running z-shell
(top of the window includes -zsh); if not change your shell with the command:
    ```
    chsh -s /bin/zsh
    ```
2. Install **Xcode** developer tools with: 
    ```
    xcode-select --install
    ```
3. Install **Homebrew** (this simplifies installing libraries required by
dynmod). 
    - If you already have Homebrew (typing `brew` does NOT return command not
    found), run: 
        ```
        brew update
        brew upgrade
        ```
    - Otherwise run:
        ```
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        ```
    - Specify the path to Homebrew by creating a **.zshrc** file: 
        - Open  a new document in BBedit and paste the following:
        ```
        export PATH=/opt/homebrew/bin:$PATH
        export PATH=/opt/homebrew/opt/python/libexec/bin:$PATH
        ```
        - Save it directly into your user folder (NOT documents), and name the
        file .zshrc  
        - Note that BBEdit will warn that: "Names that begin with a dot "." are
        reserved for the system." Click: Use "." 
    - Install the pdf-making packages dynmod needs by running:
        ```
        brew install cairo pkg-config pango
        ```
4. Install **stack** (which will manage downloading the Haskell compiler and
other associated programs) by running:
    ```
    curl -sSL https://get.haskellstack.org/ | sh
    ```
5. Open your .zshrc file with BBedit (use Options -> Show hidden items) and add:
    ```zsh
    # For Haskell Stack: 
    PATH=~/.local/bin:${PATH} 
    
    update_dynmod() {
        rm -r dynmod
        git clone https://github.com/Ravasz-Regan-Group/dynmod
        cd dynmod
        stack install
    }
    ```
6. Install and/or update **dynmod** by navigating to the place you plan to keep
the dynmod source code, then run: 
    ```
    update dynmod
    ```
    **Note**: Check on your success by running
    ```
    dynmod
    ```
    If this results in "command not found", you may need to quit and restart
    Terminal. If the issues persist, there are likely errors in your .zshrc
    file. In this case, run the commands from the `update_dynmod()` function one
    at a time.

## Windows

1. Download the Windows Installer for **stack** (which will manage downloading
    the Haskell compiler and other associated programs)
    [here](https://docs.haskellstack.org/en/stable/)
2. Open the Command line (if you are unfamiliar with it, search for "cmd")
3. To check that stack was installed, paste into the command line (if the
command is found, the output will contain useful command hints): 
    ```
    stack --help
    ```
4. Update the MSYS2 package manager pacman, then install required packages:
    
    pacman: `stack exec -- pacman -Syu`\
    git: `stack exec -- pacman -S git`\
    cairo: `stack exec -- pacman -S mingw-w64-x86_64-cairo`\
    pkg-config: `stack exec -- pacman -S mingw-w64-x86_64-pkg-config`\
    pango: `stack exec -- pacman -S mingw-w64-x86_64-pango`
5. Specify file paths to these installations: 
    - To find stack's location, enter `stack uninstall` in Command Line. This
    will not uninstall stack; merely list all the things you would delete if you
    were to uninstall it, which includes the directory path. 
    - Copy the directory containing stack’s tools. It should read:
    C:\Users\\_USERNAME_\AppData\Local\Programs\stack\
    - List the contents of this folder to find out the CPU architecture of your
    machine (e.g., x86_64-windows):
        ```
        dir C:\Users\USERNAME\AppData\Local\Programs\stack\
        ```
    - Add the folder name listed by the above command to the path to stack, then
    list its contents:
        ```
        dir C:\Users\USERNAME\AppData\Local\Programs\stack\YOUR_ARCHITECTURE\
        ```
    - Add the name of the package manager directory starting with **msys** to the
    path, append `mingw64\bin\`, then list its contents yet again (_DATE_ is the
    date of your current msys install, listed by the above command):
    C:\Users\\_USERNAME_\AppData\Local\Programs\stack\\_YOUR_ARCHITECTURE_\msys2-_DATE_\mingw64\bin\
    - Open Settings and search for "environment". Select "edit environment
    variables for your account". It must be for your account and not the system.
    - Open this, select path, then edit and paste the path you built above into
    this section; press OK.
6. Download **dynmod** from GitHub: 
    - Use the `cd` (change directory) command to navigate to the place you wish
    to keep the source code for dynmod (you can copy the file path to this
    folder by selecting it in File Explorer -> Ctrl C), then download it from
    GitHub:
        ```
        cd YOUR_FILE_PATH
        stack exec –- git clone https://github.com/Ravasz-Regan-Group/dynmod
        ```
7. Install dynmod:
    - Close and restart the command prompt. 
    - Change directory to the new folder you downloaded dynmod to:
        ```
        cd YOUR_FILE_PATH
        stack install
        ```
    - Test using the `dynmod` command. If everything is correctly installed,
    dynmod will generate a warning for a missing model file followed by use
    instructions. 

    
    
