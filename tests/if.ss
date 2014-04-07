(assert (if #t "this" 0) 'if1)
(assert (if #f 0 "this") 'if2)
(assert (if 8 "this" 1) 'if3)
(assert (if '() "this" 0) 'if4)
