;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;For gcc Emacs
(cond ((eq system-type 'darwin) (setenv "LIBRARY_PATH" "/usr/local/Cellar/libgccjit/11.3.0/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11")))
(cond ((eq system-type 'gnu/linux) (setenv "LIBRARY_PATH" "/usr/bin/gcc-10")))
(when (fboundp 'native-comp-available-p)
  (progn
    (require 'comp)
    ;; (setq comp-num-cpus 8)
    (setq package-native-compile t)
    ;; (setq native-comp-async-report-warnings-errors nil)
    (cond ((eq system-type 'darwin) (setq native-comp-compiler-options '("-O2" "-mtune=native"))))
    (cond ((eq system-type 'gnu/linux) (setq native-comp-compiler-options '("-O2" "-march=haswell" "-mtune=native"))))
    (setq native-comp-deferred-compilation t)
    (setq native-comp-compiler-options '("-O2" "-mtune=native"))
    ;; native comp of powerline throws error below
    ;; getting past  Error: List contains a loop ("22", . #0)
    ;; https://github.com/milkypostman/powerline/issues/187 - closed
    ;; (setq native-comp-deferred-compilation-deny-list '("powerline"))
    (custom-set-variables
     '(native-comp-async-report-warnings-errors 'silent))
    ))

;; Set default coding system
(set-language-environment "UTF-8")

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
