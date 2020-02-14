;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Induction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 4 + 9 + 14 + 19 + ... + (5n - 1) = (n / 2) (3 + 5n)

;; Base Case; n = 1
;; 4 = (1 / 2) (3 + 5)

;; Induction Step; Assume true for n = k, show true for n = k + 1

;; Assume: 4 + 9 + 14 + 19 + ... + (5k - 1) == (k / 2) (3 + 5k)
;; Show: 4 + 9 + 14 + 19 + ... + (5k - 1) + (5(k + 1) - 1) == ((k + 1) / 2) (3 + 5(k + 1))
;; (k / 2) (3 + 5k) + (5(k + 1) - 1) ?= ((k + 1) / 2) (3 + 5(k + 1))