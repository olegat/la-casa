(defvar silenus-sudo-passwd-cache nil)

(defun silenus-generate-tags (silenus-root overwrite)
  (interactive
   (list
    (read-directory-name "Silenus root directory: " default-directory nil t nil)
    t))
  (let ((etags "etags")
        (tags-file (file-truename (concat silenus-root "/TAGS")))
        (old-default-directory default-directory)
        (command nil))
    ;; Find etags command
    (when (string-equal system-type "windows-nt")
      (setq etags "etags.exe"))
    (setq etags (file-truename (concat invocation-directory "/" etags)))
    ;; Delete existing TAGS file (if applicable)
    (when (and (not overwrite) (file-exists-p tags-file))
      (error (concat tags-file " and overwrite is not enabled.")))
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (message "Running find with etags...")
    ;; etags write to TAGS in current working directory
    (cd silenus-root) ;; pushd
    (setq
     command
     (concat find-program " "
             silenus-root "/src "
             silenus-root "/third_party "
             "\"-(\" "
             "-iname '*.c' -or "
             "-iname '*.h' -or "
             "-iname '*.m' -or "
             "-iname '*.cpp' -or "
             "-iname '*.cc' "
             "\"-)\" "
             "-exec \"" etags "\" -a {} \";\""))
    (message (concat "Running: " command))
    (shell-command command)
    (cd old-default-directory))) ;; popd

(defun silenus-read-passwd ()
  (if silenus-sudo-passwd-cache
      silenus-sudo-passwd-cache
    (let ((passwd
           (read-passwd (concat "[sudo] password for " (getenv "USER") ": "))))
      (setq silenus-sudo-passwd-cache passwd))))

(defvar silenus-mingw-w64-x86_64-path-cache nil)

(defun silenus-read-mingw-w64-x86_64-path ()
  (if silenus-mingw-w64-x86_64-path-cache
      silenus-mingw-w64-x86_64-path-cache
    (let ((mingw-dir nil)
          (mingw-default nil)
          (mingw-gcc)
          (mingw-gcc-path)
          (PATH-sep))
      ;; Platform-specific defaults:
      (when (eq system-type 'windows-nt)
        (setq mingw-default "C:/msys64/mingw64/bin")
        (setq mingw-gcc "x86_64-w64-mingw32-gcc.exe")
        (setq PATH-sep ";"))
      (unless (eq system-type 'windows-nt)
        (setq mingw-default "/usr/bin")
        (setq mingw-gcc "x86_64-w64-mingw32-gcc")
        (setq PATH-sep ":"))
      ;; Read MinGW install directory
      (setq
       mingw-dir
       (read-directory-name
        "mingw-w64-x86-64 PATH: " mingw-default nil t nil))
      ;; Check that MinGW GCC exists
      (setq mingw-gcc-path (concat mingw-dir "/" mingw-gcc))
      (if (not (file-executable-p mingw-gcc-path))
          (error (concat "File " mingw-gcc-path " not found or not executable."))
        ;; Cache mingw-dir and prepend to PATH env
        (setq silenus-mingw-w64-x86_64-path-cache mingw-dir)
        (setenv "PATH" (concat mingw-dir PATH-sep (getenv "PATH")))))))

(defun silenus-read-steamsdk ()
  (let ((dir nil))
    (setq
     dir (read-directory-name
          "STEAMSDK= " (expand-file-name "~/SteamSDK/sdk")))
    (if (file-accessible-directory-p dir) (expand-file-name dir) nil)))


(defun silenus-compile-ggp (sh-script)
  (interactive
   (list
    (read-string
     "Compile command: "
     (concat
      "docker/build_glinux.sh --no-color"
    )
     'silenus-compile-docker-history)))
  (compile sh-script))

(defun silenus-compile-and-deploy-vapor ()
  (interactive)
  (silenus-read-mingw-w64-x86_64-path)
  ;; Read optional arg STEAMSDK
  (let ((steamsdk (silenus-read-steamsdk))
        (steamsdk-args nil))
    (when steamsdk
      (setq steamsdk-args
            (concat " -DSTEAMSDK=" steamsdk)))
    ;; Build command
    (compile
     (concat
      "cmake"
      " -G Ninja"
      steamsdk-args
      " -DCMAKE_TOOLCHAIN_FILE=" default-directory "cmake/mingw-w64-x86_64.cmake"
      " -S src/vapor"
      " -B out-vapor/"
      " && ninja -C out-vapor/"))))

(defun silenus-cs (cs-query)
  (interactive (list (read-string "cs query: " nil 'silenus-cs-query-history)))
  (cs (concat "trait:citc add_workspace:olegat/306 " cs-query)))
