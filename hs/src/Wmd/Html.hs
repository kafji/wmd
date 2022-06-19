{- |
Defines HTML pages.
-}
module Wmd.Html (
  homePage,
  searchPage,
  prefixesPage,
  errorPage,
) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Lazy qualified as TextL
import Data.Vector (Vector)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (
  Html,
  a,
  b,
  body,
  button,
  code,
  docTypeHtml,
  form,
  html,
  input,
  li,
  link,
  meta,
  p,
  toHtml,
  ul,
  (!),
 )
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (
  action,
  charset,
  href,
  method,
  rel,
  type_,
 )
import Text.Blaze.Html5.Attributes qualified as A
import Wmd.Type.SearchTarget (SearchTarget (..))

-- | Renders HTML body to HTML page as lazy text.
htmlWithBody :: H.Html -> TextL.Text
htmlWithBody b =
  renderHtml $
    docTypeHtml do
      html do
        H.head do
          meta ! charset "utf-8"
          H.title "Wmd"
          link
            ! rel "search"
            ! href "/opensearch.xml"
            ! type_ "application/opensearchdescription+xml"
            ! A.title "Wmd"
        body b

homePage :: TextL.Text
homePage = htmlWithBody do
  searchForm
  p $ a "[Prefixes]" ! A.href "/prefixes"

searchPage :: TextL.Text
searchPage = htmlWithBody mempty

prefixesPage :: Vector SearchTarget -> TextL.Text
prefixesPage targets = htmlWithBody body
  where
    body = ul (traverse_ renderPrefix targets)
    renderPrefix :: SearchTarget -> Html
    renderPrefix = li . toHtml . describePrefix
    describePrefix SearchTarget {..} = do
      b $ toHtml prefix
      toHtml $ " for " <> name <> ", "
      code $ toHtml urlTemplate

searchForm :: Html
searchForm =
  form
    ! method "GET"
    ! action "/search"
    $ do
      input
        ! type_ "text"
        ! A.name "q"
      toHtml @Text " "
      button (toHtml @Text "Go")

errorPage ::
  -- | Message.
  Text ->
  TextL.Text
errorPage msg = htmlWithBody $ p (toHtml ("error: " <> msg))
