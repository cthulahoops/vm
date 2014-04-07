(assert (if #t "this" 0))
(assert (if #f 0 "this"))
(assert (if 8 "this" 1))
(assert (if '() "this" 0))
