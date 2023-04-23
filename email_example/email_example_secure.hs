----------------------------
data UserSecret a = Erased | Active Integer a
  deriving Show

-- Monad implementation
instance Functor UserSecret where
  fmap f Erased = Erased
  fmap f (Active uid a) = Active uid (f a)

-- getters and setters
newUserSecret uid v = Active uid v
updateUserSecret (Active uid v) newVal = Active uid newVal
updateUserSecret Erased newVal = Erased
eraseUserSecret s = Erased

-- output function
displaySecret :: Show a => UserSecret a -> IO()
displaySecret Erased = do {return ()}
displaySecret (Active uid v) = do
              putStrLn $ "<user " <> (show uid) <> ">: " <> (show v)


----
type SecretsDatabase a = [UserSecret a]

eraseAllSecretsForUser :: SecretsDatabase a -> Integer -> SecretsDatabase a
eraseAllSecretsForUser db uid = do
                       secret <- db
                       case secret of
                            (Active _uid v) -> if (uid == _uid) then
                                               return Erased else
                                               return (Active _uid v)
                            Erased -> return Erased

send_bulk_email_secretly ::  SecretsDatabase String -> String -> IO()
send_bulk_email_secretly db message = do
    let emails = map (fmap (\addr -> (addr ++ ": " ++ message)))
                     db
    sequence_ [displaySecret email | email <- emails]

example_db :: SecretsDatabase String
example_db = [newUserSecret 123 "user1@example.com", newUserSecret 321 "user2@example.com"]
example_message = "hello"

main = do
     send_bulk_email_secretly example_db example_message
