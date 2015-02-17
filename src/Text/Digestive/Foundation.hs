{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Foundation
    ( FormMeta (..), FormElement (..), FormElementCfg (..)
    , StdMethod (..)
    , renderForm
    )
where

import Data.Maybe
import Data.Monoid
import Network.HTTP.Types.Method
import Lucid.Foundation
import Lucid
import Lucid.Base
import Text.Digestive
import Text.Digestive.Lucid.Html5
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


type NumberUnit = T.Text

data FormElementCfg = InputText
   | InputNumber (Maybe NumberUnit)
   | InputPassword
   | InputTextArea (Maybe Int) (Maybe Int)
   | InputHidden
   | InputSelect
   | InputRadio Bool
   | InputCheckbox
   | InputFile
   | InputDate

data FormElement = FormElement
   { fe_name :: T.Text
   , fe_label :: Maybe T.Text
   , fe_cfg :: FormElementCfg
   }

data FormMeta = FormMeta
   { fm_method :: StdMethod
   , fm_target :: T.Text
   , fm_elements :: [FormElement]
   , fm_submitText :: T.Text
   }

renderForm :: FormMeta -> View (Html ()) -> Html ()
renderForm formMeta formView =
    form_ [ makeAttribute "role" "form"
          , method_ formMethod
          , action_ formAction
          ] $
     do mconcat $ map (renderElement formView) (fm_elements formMeta)
        input_ [type_ "submit", value_ (fm_submitText formMeta)]
    where
      formMethod = T.decodeUtf8 $ renderStdMethod (fm_method formMeta)
      formAction = fm_target formMeta

renderElement :: View (Html ()) -> FormElement -> Html ()
renderElement formView formElement =
    div_ [] $
    do case errors (fe_name formElement) formView of
         [] -> mempty
         errorMsgs ->
             div_ [class_ alert_box_] $ ul_ [] $ mapM_ (li_ []) errorMsgs
       case fe_label formElement of
         Just lbl ->
             label_ [name_ $ fe_name formElement] $ toHtmlRaw lbl
         Nothing ->
             mempty
       let ct = buildFun (fe_name formElement) formView
       if hasAddon
       then div_ [class_ "input-group"] (ct >>= \_ -> groupAddonAfter)
       else ct
    where
      (hasAddon, groupAddonAfter) =
          case fe_cfg formElement of
            InputNumber (Just numberUnit) ->
                (True, span_ [class_ "input-group-addon"] $ toHtmlRaw numberUnit)
            _ ->
                (False, mempty)
      buildFun =
          case fe_cfg formElement of
            InputText -> inputText
            InputPassword -> inputPassword
            InputTextArea taRows taCols -> inputTextArea taRows taCols
            InputHidden -> inputHidden
            InputSelect -> inputSelect
            InputRadio rBr -> inputRadio rBr
            InputCheckbox -> inputCheckbox
            InputFile -> inputFile
            InputNumber _ -> inputX "number"
            InputDate -> inputX "date"

inputX :: T.Text -> T.Text -> View v -> Html ()
inputX x ref view =
    input_ $ [ type_ x
             , id_ ref'
             , name_ ref'
             , value_ (fieldInputText ref view)
             ] ++ (ifSingleton (x == "number") $ step_ "any")
  where
    ref' = absoluteRef ref view
