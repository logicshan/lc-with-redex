#lang scheme
(define x '((0 0) (0 1) (1 0) (0 2) (1 1) (2 0) (0 3) (1 2) (2 1) (3 0) (0 4) (1 3) (2 2) (3 1) (4 0) (0 5) (1 4) (2 3) (3 2) (4 1) (5 0) (0 6) (1 5) (2 4) (3 3) (4 2) (5 1) (6 0) (0 7) (1 6) (2 5) (3 4) (4 3) (5 2) (6 1) (7 0) (0 8) (1 7) (2 6) (3 5) (4 4) (5 3) (6 2) (7 1) (8 0) (0 9) (1 8) (2 7) (3 6) (4 5) (5 4) (6 3) (7 2) (8 1) (9 0) (0 10) (1 9) (2 8) (3 7) (4 6) (5 5) (6 4) (7 3) (8 2) (9 1) (10 0) (0 11) (1 10) (2 9) (3 8) (4 7) (5 6) (6 5) (7 4) (8 3) (9 2) (10 1) (11 0) (0 12) (1 11) (2 10) (3 9) (4 8) (5 7) (6 6) (7 5) (8 4) (9 3) (10 2) (11 1) (12 0) (0 13) (1 12) (2 11) (3 10) (4 9) (5 8) (6 7) (7 6) (8 5) (9 4) (10 3) (11 2) (12 1) (13 0) (0 14) (1 13) (2 12) (3 11) (4 10) (5 9) (6 8) (7 7) (8 6) (9 5) (10 4) (11 3) (12 2) (13 1) (14 0) (0 15) (1 14) (2 13) (3 12) (4 11) (5 10) (6 9) (7 8) (8 7) (9 6) (10 5) (11 4) (12 3) (13 2) (14 1) (15 0) (0 16) (1 15) (2 14) (3 13) (4 12) (5 11) (6 10) (7 9) (8 8) (9 7) (10 6) (11 5) (12 4) (13 3) (14 2) (15 1) (16 0) (0 17) (1 16) (2 15) (3 14) (4 13) (5 12) (6 11) (7 10) (8 9) (9 8) (10 7) (11 6) (12 5) (13 4) (14 3) (15 2) (16 1) (17 0) (0 18) (1 17) (2 16) (3 15) (4 14) (5 13) (6 12) (7 11) (8 10) (9 9) (10 8) (11 7) (12 6) (13 5) (14 4) (15 3) (16 2) (17 1) (18 0) (0 19) (1 18) (2 17) (3 16) (4 15) (5 14) (6 13) (7 12) (8 11) (9 10) (10 9) (11 8) (12 7) (13 6) (14 5) (15 4) (16 3) (17 2) (18 1) (19 0) (0 20) (1 19) (2 18) (3 17) (4 16) (5 15) (6 14) (7 13) (8 12) (9 11) (10 10) (11 9) (12 8) (13 7) (14 6) (15 5) (16 4) (17 3) (18 2) (19 1) (20 0) (0 21) (1 20) (2 19) (3 18) (4 17) (5 16) (6 15) (7 14) (8 13) (9 12) (10 11) (11 10) (12 9) (13 8) (14 7) (15 6) (16 5) (17 4) (18 3) (19 2) (20 1) (21 0) (0 22) (1 21) (2 20) (3 19) (4 18) (5 17) (6 16) (7 15) (8 14) (9 13) (10 12) (11 11) (12 10) (13 9) (14 8) (15 7) (16 6) (17 5) (18 4) (19 3) (20 2) (21 1) (22 0) (0 23) (1 22) (2 21) (3 20) (4 19) (5 18) (6 17) (7 16) (8 15) (9 14) (10 13) (11 12) (12 11) (13 10) (14 9) (15 8) (16 7) (17 6) (18 5) (19 4) (20 3) (21 2) (22 1) (23 0) (0 24) (1 23) (2 22) (3 21) (4 20) (5 19) (6 18) (7 17) (8 16) (9 15) (10 14) (11 13) (12 12) (13 11) (14 10) (15 9) (16 8) (17 7) (18 6) (19 5) (20 4) (21 3) (22 2) (23 1) (24 0) (0 25) (1 24) (2 23) (3 22) (4 21) (5 20) (6 19) (7 18) (8 17) (9 16) (10 15) (11 14) (12 13) (13 12) (14 11) (15 10) (16 9) (17 8) (18 7) (19 6) (20 5) (21 4) (22 3) (23 2) (24 1) (25 0) (0 26) (1 25) (2 24) (3 23) (4 22) (5 21) (6 20) (7 19) (8 18) (9 17) (10 16) (11 15) (12 14) (13 13) (14 12) (15 11) (16 10) (17 9) (18 8) (19 7) (20 6) (21 5) (22 4) (23 3) (24 2) (25 1) (26 0) (6 21) (7 20) (8 19) (9 18) (10 17) (11 16) (12 15) (13 14) (14 13) (15 12) (16 11) (17 10) (18 9) (19 8) (20 7) (21 6) (22 5) (23 4) (24 3) (25 2) (26 1) (27 0)))
(require "../my-scheme/fmt/fmt.ss")
((fmt 'cur "us'('*(10('('usi2xi2')')/)')'") x)