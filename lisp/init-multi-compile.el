;;;init-multi-compile.el --- Initialize key-bindings for multi-compile.	-*- lexical-binding: t -*-

(use-package multi-compile
  :custom
  (multi-compile-completion-system 'ivy)
  (multi-compile-alist '(
                         (go-mode . (("gr" "go run -v"
                                      (buffer-file-name))
                                     ("gb" "go build -v"
                                      (projectile-project-root))
                                     ("gbr" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                                      (projectile-project-root))))
                         (scala-mode . (("scl" "scala"
                                         (buffer-file-name))
                                        ("sbrun" "sbt run"
                                         (projectile-project-root))
                                        ("sbpack" "sbt package"
                                         (projectile-project-root))
                                        ("sbass" "sbt assembly"
                                         (projectile-project-root))
                                        ("sbtall" "sbt compile; sbt test; sbt run"
                                         (projectile-project-root))))
                         ;; (rust-mode . (("rw" "cargo build --release -q"
                         ;;                (locate-dominating-file buffer-file-name ".git"))
                         ;;               ("rb" "cargo build --release --color never"
                         ;;                (locate-dominating-file buffer-file-name ".git"))
                         ;;               ("rbr" "cargo run --release --color never"
                         ;;                (multi-compile-locate-file-dir ".git"))))
                         (java-mode . (("mpac" "mvn clean package"
                                        (projectile-project-root))
                                       ("mtest" "mvn validate"
                                        (projectile-project-root))
                                       ("gbuild" "gradle clean build"
                                        (projectile-project-root))
                                       ("gjar" "gradle clean jar"
                                        (projectile-project-root))))
                         )
                       ))

  (provide 'init-multi-compile)
