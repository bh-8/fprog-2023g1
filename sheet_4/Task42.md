### 4.2. Underscores, Types
1. The Underscore has a special meaning in haskells function definitions. Given a programming language **N** of your choice answer the following questions:
    - `_` in Haskell is used whenever something is not needed and can be omitted; let **N** be **C**:
    - Is `_` a legal expression or name in **N**?
        - `_` is no legal expression in **C**, but can appear in names or identifiers
    - If it is a legal expression/name: what are the differences in its use compared to the underscore in haskell? If it isn’t a legal expression/name: is there a different construct in **N** that is similar to how `_` is used in haskell?
        - afaik, there is no similar construct available
    - Discuss the differences of the following two expressions in *ghci*: `let f _ _ = undefined` and `let g x x = undefined`
        - underscore avoids error due to conflicting definitions
2. The prelude of *ghci* defines the functions `foldl1` and `foldr1`. What are the differences of those two functions to their normal counterparts `foldl`/`foldr`?
    - `foldr`: it takes the **second argument and the last item** of the list and applies the function, then it takes the **penultimate item from the end and the result**, and so on
    - `foldr1`: it takes the **last two items** of the list and applies the function, then it takes the **third item from the end and the result**, and so on
    - `foldl`: it takes the **second argument and the first item** of the list and applies the function to them, then **feeds the function with this result and the second argument** and so on
    - `foldl1`: it takes the **first two items** of the list and applies the function to them, then **feeds the function with this result and the third argument** and so on
