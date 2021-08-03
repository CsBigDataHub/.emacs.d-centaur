;; init-calendar.el --- Initialize calendar configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;; Calendar configuration.
;;

;;; Code:

(require 'init-custom)

;; to add indian holidays
;; (eval-when-compile
;;   (require 'calendar)
;;   (require 'holidays))

;; ;;;###autoload
;; (defvar holiday-indian-holidays
;;   '((holiday-fixed 1 1   "English New Year")
;;     (holiday-fixed 1 15  "Pongal, Mahar Sankranthi")
;;     (holiday-fixed 1 26  "Republic Day")

;;     (holiday-fixed 3 7   "Maha Shivaratri")
;;     (holiday-fixed 3 24  "Holi")
;;     (holiday-fixed 3 25  "Good Friday")
;;     (holiday-fixed 3 27  "Easter")

;;     (holiday-fixed 4 1   "Bank's Holiday")
;;     (holiday-fixed 4 14	 "Ambedkar Jayanti")
;;     (holiday-fixed 4 15  "Rama Navami")

;;     (holiday-fixed 7 7   "Eid al-Fitr, Ramadan")

;;     (holiday-fixed 8 15  "Independence Day")
;;     (holiday-fixed 8 18  "Rakhi, Raksha Bandhan")
;;     (holiday-fixed 8 25  "Krishna Janmashtami")

;;     (holiday-fixed 9 5   "Ganesh Chaturthi")

;;     (holiday-fixed 10 2  "Gandhi Jayanti")
;;     (holiday-fixed 10 3  "Al-Hijra, Islamic New Year")
;;     (holiday-fixed 10 11 "Dussehra, Madhvacharya Jayanti")
;;     (holiday-fixed 10 30 "Diwali, Lakshmi Puja")
;;     (holiday-fixed 12 25 "Merry Christmas"))
;;   "Indian Holidays")

;; (setq calendar-holidays (append calendar-holidays holiday-indian-holidays))

;; Chinese calendar
;; `pC' can show lunar details
;; (when centaur-chinese-calendar
;;   (use-package cal-china-x
;;     :after calendar
;;     :commands cal-china-x-setup
;;     :init (cal-china-x-setup)
;;     :config
;;     ;; Holidays
;;     (setq calendar-mark-holidays-flag t
;;           cal-china-x-important-holidays cal-china-x-chinese-holidays
;;           cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
;;                                          (holiday-lunar 7 7 "七夕节")
;;                                          (holiday-fixed 3 8 "妇女节")
;;                                          (holiday-fixed 3 12 "植树节")
;;                                          (holiday-fixed 5 4 "青年节")
;;                                          (holiday-fixed 6 1 "儿童节")
;;                                          (holiday-fixed 9 10 "教师节"))
;;           holiday-other-holidays '((holiday-fixed 2 14 "情人节")
;;                                    (holiday-fixed 4 1 "愚人节")
;;                                    (holiday-fixed 12 25 "圣诞节")
;;                                    (holiday-float 5 0 2 "母亲节")
;;                                    (holiday-float 6 0 3 "父亲节")
;;                                    (holiday-float 11 4 4 "感恩节"))
;;           calendar-holidays (append cal-china-x-important-holidays
;;                                     cal-china-x-general-holidays
;;                                     holiday-other-holidays))))

(use-package calendar
  :ensure nil
  :custom
  ((calendar-holidays holiday-general-holidays)
   (calendar-mark-holidays-flag t)
   (holiday-general-holidays ;; US public holidays
    '((holiday-fixed 1 1 "New Year's Day")
      (holiday-float 1 1 3 "Martin Luther King Day")
      (holiday-float 2 1 3 "President's Day")
      (holiday-float 5 1 -1 "Memorial Day")
      (holiday-fixed 7 4 "Independence Day")
      (holiday-float 9 1 1 "Labor Day")
      (holiday-float 10 1 2 "Columbus Day")
      (holiday-fixed 11 11 "Veteran's Day")
      (holiday-fixed 12 25 "Christmas")
      (holiday-float 11 4 4 "Thanksgiving")))
   (calendar-time-zone -360)
   (calendar-latitude 45.018270)
   (calendar-longitude -93.473890)
   (calendar-standard-time-zone-name "CST")
   (calendar-daylight-time-zone-name "CDT")

   )
  :config
  (defadvice calendar-generate-month
      (after highlight-weekend-days (month year indent) activate)
    "Highlight weekend days"
    (dotimes (i 31)
      (let ((date (list month (1+ i) year)))
        (if (or (= (calendar-day-of-week date) 0)
                (= (calendar-day-of-week date) 6))
            (calendar-mark-visible-date date 'font-lock-doc-string-face)))))

  ;; https://stackoverflow.com/questions/23566000/how-to-count-days-excluding-weekends-and-holidays-in-emacs-calendar
  (eval-after-load "calendar"
    `(progn
       (require 'holidays)
       (defun my-calendar-count-days(d1 d2)
         (let* ((days (- (calendar-absolute-from-gregorian d1)
                         (calendar-absolute-from-gregorian d2)))
                (days (1+ (if (> days 0) days (- days)))))
           days))

       (defun my-calendar-count-holidays-on-weekdays-in-range (start end)
         (let ((holidays (holiday-in-range start end))
               (counter 0))
           (dolist (element holidays)
             (let ((day (calendar-day-of-week (car element))))
               (if (and (> day 0)
                        (< day 6))
                   (incf counter))))
           counter))

       (defun my-calendar-count-weekend-days(date1 date2)
         (let* ((tmp-date (if (< date1 date2) date1 date2))
                (end-date (if (> date1 date2) date1 date2))
                (weekend-days 0))
           (while (<= tmp-date end-date)
             (let ((day-of-week (calendar-day-of-week
                                 (calendar-gregorian-from-absolute tmp-date))))
               (if (or (= day-of-week 0)
                       (= day-of-week 6))
                   (incf weekend-days ))
               (incf tmp-date)))
           weekend-days))

       (defun calendar-count-days-region2 ()
         "Count the number of days (inclusive) between point and the mark
  excluding weekends and holidays."
         (interactive)
         (let* ((d1 (calendar-cursor-to-date t))
                (d2 (car calendar-mark-ring))
                (date1 (calendar-absolute-from-gregorian d1))
                (date2 (calendar-absolute-from-gregorian d2))
                (start-date (if (<  date1 date2) date1 date2))
                (end-date (if (> date1 date2) date1 date2))
                (days (- (my-calendar-count-days d1 d2)
                         (+ (my-calendar-count-weekend-days start-date end-date)
                            (my-calendar-count-holidays-on-weekdays-in-range
                             start-date end-date)))))
           (message "Region has %d workday%s (inclusive)"
                    days (if (> days 1) "s" ""))))
       (define-key calendar-mode-map (kbd "M-s-=") 'calendar-count-days-region2)
       )))

(provide 'init-calendar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
