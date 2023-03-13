-- transformers
import Control.Monad.Trans.Maybe


stringy :: String ->  MaybeT IO String
stringy =  MaybeT . pure .  Just

failureN :: MaybeT IO String
failureN = MaybeT . pure $  Nothing

failureE :: String -> MaybeT IO String
failureE = MaybeT . ioError . userError


let x1 = [stringy "hello", stringy "there", failureN, failureE "boom1", stringy "you"]
> runMaybeT $ asum x1
> Just "hello"

---

let x2 = [failureN, failureE "boom1", stringy "hello!"]
> runMaybeT $ asum x2
> *** Exception: user error (boom1)

----

let x3 = [failureN, failureN, stringy "hello!"]
> runMaybeT $ asum x3
> Just "hello!"

------


let x1 :: [IO (Maybe Int)] = [pure Nothing, pure $ Just 10, pure Nothing] in
foldr (liftA2 (<|>)) (pure $ error "oh noooo") x1

let x3 :: [IO (Maybe Int)] = [ioError (userError "boom"), pure Nothing, pure $ Just 10, pure Nothing] in
foldr (liftA2 (<|>)) (pure $ error "oh noooo") x2