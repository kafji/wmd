{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Wmd.HTML (
  homePage,
  searchPage,
) where

import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (
  a,
  body,
  docTypeHtml,
  form,
  h1,
  html,
  input,
  link,
  meta,
  (!),
 )
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (
  charset,
  href,
  name,
  rel,
  type_,
  value,
 )
import Text.Blaze.Html5.Attributes qualified as A

homePage :: Text
homePage = withBody do
  h1 "Wmd"
  a "[Search]" ! A.href "/search"

searchPage :: Text
searchPage = withBody do
  form do
    input
      ! type_ "text"
      ! name "q"
    input
      ! type_ "submit"
      ! value "Search"
  a "[Home]" ! href "../"

withBody :: H.Html -> Text
withBody b =
  LazyText.toStrict $
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
