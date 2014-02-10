nil [ drop ` flip drop ` flip drop ] , :car def
nil [ drop ` flip drop ` drop ] , :cdr def
nil [ drop ` flip drop nil = ] , :null? def
nil [ drop ` flip drop ? ] , :vm? def
nil [ drop ` flip drop show ] , :str def
nil [ drop ` flip ` flip drop flip , ] , :cons def
nil [ drop ` flip ` flip drop * ] , :vm* def
nil [ drop ` flip ` flip drop + ] , :vm+ def
nil [ drop ` flip ` flip drop - ] , :- def
nil [ drop ` flip ` flip drop < ] , :< def
nil [ drop ` flip ` flip drop > ] , :> def
nil [ drop ` flip ` flip drop <= ] , :<= def
nil [ drop ` flip ` flip drop >= ] , :>= def
nil [ drop ` flip ` flip drop = ] , := def
