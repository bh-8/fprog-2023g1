### 5.2. The Functor class and its limits
1. Out of the following types which ones can be instances of the `Functor` class? Which ones can't and why?
    1. `data Bool_ = True_ | False_`
        - no, no type variable specified, therefore signature does not match `* -> *`, instead it is just `*`
    2. `data List a = List a (List a) | Empty`
        - yes
    3. `data Either_ a b = Right_ a | Left_ b`
        - no, to many type variables specified, therefore signature does not match `* -> *`, instead it is `* -> * -> *`
    4. `data Maybe_ a = Nothing_ | Just_ a`
        - yes
    5. `data Pair a b = Pair a b`
        - no, to many type variables specified, therefore signature does not match `* -> *`, instead it is `* -> * -> *`
    6. `data LList a = LList [a] (a, a) a`
        - yes

2. Is there a way to make them fit the `Functor` class requirements?
    - `data Bool_ = True_ | False_` --> `data Bool_ a = True_ | False_`
    - `data Either_ a b = Right_ a | Left_ b` --> `data Either_ a = Right_ a | Left_ a`
    - `data Pair a b = Pair a b` --> `data Pair a = Pair a a`