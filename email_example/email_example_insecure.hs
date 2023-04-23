
type EmailDatabase = [(Integer, String)]

makeNewUser :: EmailDatabase -> Integer -> String -> EmailDatabase
updateUser :: EmailDatabase -> Integer -> String -> EmailDatabase
deleteUser :: EmailDatabase -> Integer -> EmailDatabase
sendBulkEmail :: EmailDatabase -> String -> IO()

----------------------------

makeNewUser db uid addr = (uid, addr) : db
deleteUser db uid = [(_uid, addr) | (_uid, addr) <- db, uid /= _uid]
updateUser db uid addr = (uid, addr) : (deleteUser db uid)

sendBulkEmail db message = do
              let emails = [addr ++ ": " ++ message | (_, addr) <- db]
              sequence_ [print email | email <- emails]

example_db :: EmailDatabase
example_db = [(123, "user1@example.com"), (321, "user2@example.com")]
example_message = "hello"

main = do
     sendBulkEmail example_db example_message
