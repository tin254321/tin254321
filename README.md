SUB Gauss (a, b, n, x, tol, er)
         DIMENSION s (n)
          er = 0
         DOFOR i = 1, n
                  s = ABS (a,1)
                  DOFOR j = 2, n
                          IF ABS(a) > s   THEN S = ABS (a.)
                   END DO
         END DO
          CALL Eliminate(a, s, n, b, tol, er)
           IF er 1 THEN
                   CALL Substitute(a, n, b. x)
          END IF
END Gauss

SUB Eliminate (a. s. n. b. tol, er)
         DOFOR K = 1, n - 1
                   CALL Pivot (a, b, s, n. k)
                    IF ABS (a / s) < tol THEN
                            er = -1
                            Exit do
                    END IF
                    DOFOR i = k + 1, n
                             factor = a / a
                             DOFOR j = k + 1, n
                                   a = a - factor * b
                             END DO
                              b =  b - factor * b
                     END DO
            END DO
            IF ABS(a / s) < tol THEN er = -1
   END Eliminate



SUB Pivot (a, b, s, n, k)
       p = k
       big =  ABS (a / S)
       DOFOR i i =k + 1, n
                dummy = ABS (a / S)
                IF dummy > big THEN
                       big = dummy
                       p = ii
               END IF
        END DO
        IF p diferente k THEN
               DOFOR jj = k, n
                      dummy = a
                       a = a
                       a =  dummy
               END DO
               dummy = b
                b = b
                b = dummy
               dummy = s
               s = s
               s = dummy
         END IF
END pivot

SUB Substitute (a, n, b, x)
         X = b / a
        DOFOR i = n - 1, 1, -1
               sum = 0
               Do for j = I + 1, n
                       sum = sum + a * X
               END DO
               X = (b - sum) / a
       END DO (b; sum) a₁.1
END Substitute

