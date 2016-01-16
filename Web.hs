{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
  [ dir "echo"    $ echo
  , dir "query"   $ queryParams
  , dir "form"    $ formPage
  , dir "fortune" $ fortune
  , dir "files"   $ fileServing
  , dir "upload"  $ upload
  , homePage
  ]

--Blaze||Heist||HSP
template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "На главную"  

homePage :: ServerPart Response
homePage =
    ok $ template "Главная страница" $ do
           H.h1 "Привет!"
           H.p "Писать приложения на Happstack Lite быстро и просто!"
           H.p "Зацени эти крутые приложения:"
           H.p $ a ! href "/echo/secret%20message"  $ "Эхо"
           H.p $ a ! href "/query?foo=bar" $ "Параметры запроса"
           H.p $ a ! href "/form"          $ "Обработка формы"
           H.p $ a ! href "/fortune"       $ "Печеньки-предсказания (куки)"
           H.p $ a ! href "/files"         $ "Доступ к файлам"
           H.p $ a ! href "/upload"        $ "Размещение файлов"

--Dynamic Path Segments
echo :: ServerPart Response
echo =
    path $ \(msg :: String) ->
        ok $ template "Эхо" $ do
          p $ "Динамическая часть адреса: " >> toHtml msg
          p "Измени адрес страницы, чтобы вывести на экран что-то иное. test http://localhost:8000/echo/fantastic"           

--Query String Parameters
queryParams :: ServerPart Response
queryParams =
    do mFoo <- optional $ lookText "foo"
       ok $ template "Параметры запроса" $ do
         p $ "foo = " >> toHtml (show mFoo)
         p $ "Измени адрес страницы, чтобы установить другое значение foo. test http://localhost:8000/query?foo=bar"

--We can use lookText (and friends) to extract values from forms as well
formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           ok $ template "form" $
              form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
                label ! A.for "msg" $ "Напиши что-нибудь умное"
                input ! type_ "text" ! A.id "msg" ! name "msg"
                input ! type_ "submit" ! value "Отправить"
    processForm :: ServerPart Response
    processForm =
        do method POST
           msg <- lookText "msg"
           ok $ template "form" $ do
             H.p "Ты написал:"
             H.p (toHtml msg)

--Cookies
--TODO check cookies
fortune :: ServerPart Response
fortune = msum [ viewFortune, updateFortune ]
    where
      viewFortune :: ServerPart Response
      viewFortune =
          do method GET
             mMemory <- optional $ lookCookieValue "Печеньки-предсказания (куки)"
             let memory = fromMaybe "Твое будущее будет определено с помощью веб-технологий!" mMemory
             ok $ template "fortune" $ do
                    H.p "Сообщение из твоей печеньки-предсказания (куки):"
                    H.p (toHtml memory)
                    form ! action "/fortune" ! enctype "multipart/form-data" ! A.method "POST" $ do
                    label ! A.for "fortune" $ "Измени свою судьбу: "
                    input ! type_ "text" ! A.id "fortune" ! name "new_fortune"
                    input ! type_ "submit" ! value "Отправить"
      updateFortune :: ServerPart Response
      updateFortune =
          do method POST
             fortune <- lookText "new_fortune"
             addCookies [(Session, mkCookie "fortune" (unpack fortune))]
             seeOther ("/fortune" :: String) (toResponse ())

--File Serving
--TODO print file list
fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."

--Handling file uploads is very straight forward. We create a form, just as before. Except instead of lookText we use lookFile.
--When a file is uploaded, we store it in a temporary location.
--The temporary file will automatically be deleted after the server has sent the response.
--That ensures that unused files don't clutter up the disk.
--In most cases, you don't want a user to upload a file just to have it deleted.
--Normally the upload handler would use moveFile or copyFile to move (or copy) the temporary file to a permanent location.
--TODO save uploaded file to DB
upload :: ServerPart Response
upload =
       msum [ uploadForm
            , handleUpload
            ]
    where
    uploadForm :: ServerPart Response
    uploadForm =
        do method GET
           ok $ template "Размещение файла" $ do
             form ! enctype "multipart/form-data" ! A.method "POST" ! action "/upload" $ do
               input ! type_ "file" ! name "file_upload" ! size "40"
               input ! type_ "submit" ! value "upload"
    handleUpload :: ServerPart Response
    handleUpload =
        do (tmpFile, uploadName, contentType) <- lookFile "file_upload"
           ok $ template "Файл загружен" $ do
                p (toHtml $ "Временный файл: " ++ tmpFile)
                p (toHtml $ "Имя загрузки:  " ++ uploadName)
                p (toHtml $ "Тип контента:   " ++ show contentType)
