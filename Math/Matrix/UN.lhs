>module Math.Matrix.UN where
>
>data Field a = Field {
>   un_resolution_symbol :: a,
>   link_to_link :: a,
>   meeting_symbol :: a,
>   title :: a,
>   related_document :: a,
>   vote_notes :: a,
>   vote_date  :: a,
>   agenda_information :: a,
>   agenda_information_link :: a,
>   voting_summary :: a,
>   country_votes :: Map Country a }

>data Country = Country { country_name :: String }
>  deriving (Eq,Ord)

>splitFields :: String -> [String]
>splitFields lst = v : if r == [] then [] else splitFields r
>   where (v,r) = break (== '\t') lst

>readData countries handle = do
>  atEOF <- hIsEOF handle
>  if atEOF then return []
>           else do
>                 fld <- readField countries handle
>                 rest <- readData countries handle
>                 return (fld:rest)

>readField countries handle = do
>   line <- hGetLine handle
>   let flds = splitFields line
>   let ([res,lin,mee,tit,rel,vnotes,vdate,agenda,agenda_link,summary],countriesStr)
>         = splitAt 10 flds
>   let voteMap = Map.fromList (zip countries countriesStr)
>   let res = Field { un_resolution_symbol = res,
>                     link_to_link = lin,
>                     meeting_symbol = mee,
>                     title = tit,
>                     related_document = rel,
>                     vote_notes = vnotes,
>                     vote_date = vdate,
>                     agenda_information = agenda,
>                     agenda_information_link = agenda_link,
>                     voting_summary = summary,
>                     country_votes = voteMap }

>readCSV :: Handle -> IO (Field String, ([] :*: Field) String)
>readCSV handle = do
>  line <- hGetLine handle
>  let flds = splitFields line
>  let ([res,lin,mee,tit,rel,vnotes,vdate,agenda,agenda_link,summary],countriesStr)
>        = splitAt 10 flds
>  let countries = Map.fromList $ zip (map Country countriesStr) countriesStr
>  let header = Field { un_resolution_symbol = res,
>                       link_to_link = lin,
>                       meeting_symbol = mee,
>                       title = tit,
>                       related_document = rel,
>                       vote_notes = vnotes,
>                       vote_date = vdate,
>                       agenda_information = agenda,
>                       agenda_information_link = agenda_link,
>                       voting_summary = summary,
>                       country_votes = countries }
>  dataList <- readData countries handle
>  let m = Matrix dataList
>  return (header,m)
>
>main :: IO ()
>main = do
>   handle <- openFile "votes.csv" ReadMode
>   (header,matrix) <- readCSV handle
>   putStrLn (matrix <!> ((!! 0), \v -> country_votes v `Map.!` "angola")) 
