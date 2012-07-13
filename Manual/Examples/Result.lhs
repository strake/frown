> module Result where
>
> data Result a                 =  Return a | Fail String
>                                  deriving (Show)
>
> instance Monad Result where
>     return                    =  Return
>     Fail s   >>= k            =  Fail s
>     Return a >>= k            =  k a
>     fail                      =  Fail