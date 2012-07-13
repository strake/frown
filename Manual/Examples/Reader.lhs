> module Reader where
>
> newtype Reader env a          =   Reader { apply :: env -> a }
>
> instance Monad (Reader env) where
>     return a                  =   Reader (\ env -> a)
>     m >>= k                   =   Reader (\ env -> apply (k (apply m env)) env)
>     fail s                    =   Reader (error s)
>
> getenv                        ::  Reader env env
> getenv                        =   Reader (\ env -> env)
>
> withenv                       ::  env -> Reader env a -> Reader env' a
> withenv env m                 =   Reader (\ env' -> apply m env)