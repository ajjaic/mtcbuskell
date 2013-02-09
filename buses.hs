
import Busmtc
import Sqdbase
import Database.HDBC.Sqlite3
import Database.HDBC 

databaseName = "bus.db"

main = connectSqlite3 databaseName >>= (\c -> withTransaction c popDb)

popDb conn = do
    prepExec conn createTableQ []
    a <- getBusList busListUrl
    let b = take 3 a
    mapM_ (putIntoDB conn) b
