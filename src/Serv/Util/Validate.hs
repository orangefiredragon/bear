module Serv.Util.Validate where

import           Control.Applicative


data Validate a = Invalid [String] | Valid a

runValidate :: Validate a -> Either [String] a
runValidate (Valid   x)  = Right x
runValidate (Invalid es) = Left es

instance Functor Validate where
    fmap f (Valid x)    = Valid (f x)
    fmap _ (Invalid es) = Invalid es

instance Applicative Validate where
    pure = Valid
    Valid f     <*> Valid x     = Valid (f x)
    Invalid es1 <*> Valid _     = Invalid es1
    Valid _     <*> Invalid es2 = Invalid es2
    Invalid es1 <*> Invalid es2 = Invalid (es1++es2)

instance Monad Validate where
    return = pure
    Valid x    >>= f = f x
    Invalid es >>= _ = Invalid es

    fail e = Invalid [e]
