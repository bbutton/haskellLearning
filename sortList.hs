sortListByLength a b
                 | length a < length b = LT
                 | length a == length b = EQ
                 | length a > length b = GT


