--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mappend)
import           Data.List   (intercalate)
import           Hakyll
import           System.FilePath                 ((</>), takeBaseName, takeDirectory)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*/*/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*/*/examples/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    cats <- buildCategories "posts/*/*" (fromCapture "posts/*/index.html")

    match "posts/*/*/*.md" $ do
        route $ setExtension "html"
        compile $ do
          let indexCtx =
                field "tags" (\_ -> renderCats cats) <>
                postCtx

          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    indexCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    match "posts/*/index.md" $ do
        route $ setExtension "html"
        compile $ do
            identifier <- getUnderlying
            posts <- recentFirst =<< loadAll (fromGlob $ (takeDirectory . toFilePath $ identifier) ++ "/*/*.md")
            let indexCtx =
                  listField "posts" postCtx (return posts) <>
                  field "tags" (\_ -> renderCats cats)     <>
                  defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*/*/*.md"
            let indexCtx =
                  listField "posts" postCtx (return posts) <>
                  field "tags" (\_ -> renderCats cats)     <>
                  defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "about.html" $ do
        route idRoute
        compile $ do
            let aboutCtx =
                  field "tags" (\_ -> renderCats cats) <>
                  defaultContext

            getResourceBody
                >>= applyAsTemplate aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                >>= relativizeUrls

    -- keep it the same path as the original howistart.org
    create ["static/posts.rss"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*/*/*.md" "content"
        renderAtom myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "How I Start."
    , feedDescription = "How I Start is a mix between a collection of development tutorials and The Setup."
    , feedAuthorName  = "Tristan Sloughter"
    , feedAuthorEmail = "contact@howistart.org"
    , feedRoot        = "http://www.howistart.org"
    }

postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , field "basename" (\item -> return . takeDirectory . toFilePath $ itemIdentifier item)
    , defaultContext
    ]

renderCats :: Tags -> Compiler (String)
renderCats = renderTags makeLink (intercalate "")
  where
    makeLink tag url count _ _ = renderHtml $
        H.li ! A.class_ "category" $ H.a ! A.href (toValue ("/posts" </> tag)) $ toHtml tag
