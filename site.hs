{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

-- Main process
main :: IO ()
main = hakyll $ do

    -- Templates compilation
    match templatesRule $ compile templateBodyCompiler

    -- Static images
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Static pictograms
    match ("picto/*.png" .||. "picto/*.svg") $ do
      route   idRoute
      compile copyFileCompiler

    -- Css for styles and fonts
    match ("css/*.css" .||. "fonts/*/*.css") $ do
      route   idRoute
      compile compressCssCompiler

    -- Custom fonts
    match ("fonts/*/*.woff" .||. "fonts/*/*.woff2") $ do
      route   idRoute
      compile copyFileCompiler

    -- Seeded JSON files (from Planet)
    match "_seeds/api/*.json" $ do
      route (unseedRoute `composeRoutes` setExtension "json")
      compile copyFileCompiler

    -- JSOO Artifacts
    match "_seeds/*.bc.js" $ do
      route (unseedRoute `composeRoutes` setExtension "js")
      compile copyFileCompiler

    -- Project seeded (from Planet)
    match projectsRule $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/project.html" projectContext
        >>= loadAndApplyTemplate "templates/default.html" projectContext
        >>= relativizeUrls

    -- Long Stories seeded (from Planet)
    match longsRule $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    -- Short Stories seeded (from Planet)
    match shortsRule $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls    

    -- Journal.html
    match "pages/*.html" $ do
      route (truncateRoute "pages/" `composeRoutes` setExtension "html")
      compile $ do
        getResourceBody
        >>= applyAsTemplate defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
      
    -- Index.html
    match "index.html" $ do
      route idRoute
      compile $ do
        
        projects <- (recentFirst =<< loadAll projectsRule)
        longs <- (recentFirst =<< loadAll longsRule)
        shorts <- (recentFirst =<< loadAll shortsRule)
        
        let indexContext =
              listField "projects" projectContext (return projects) `mappend`
              listField "shorts" defaultContext (return shorts) `mappend`
              listField "longs" defaultContext (return longs) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "templates/default.html" indexContext
          >>= relativizeUrls



-- Remove artifacts in routes
truncateRoute fragment = gsubRoute fragment (const "")
unseedRoute = truncateRoute "_seeds/"

-- Custom Contexts

projectContext :: Context String
projectContext =
  bodyField      "body"              `mappend`
  titleField     "title"             `mappend`
  titleField     "name"              `mappend`
  teaserField    "teaser" "content"  `mappend`
  urlField       "url"               `mappend`
  pathField      "path"              `mappend`
  metadataField                      `mappend`
  missingField

-- Rules

projectsRule =
  "_seeds/projects/*.org"
  .||. "_seeds/projects/*.md"
  .||. "_seeds/projects/*.txt"

longsRule =
  "_seeds/longs/*.org"
  .||. "_seeds/longs/*.md"
  .||. "_seeds/longs/*.txt"

shortsRule =
  "_seeds/shorts/*.org"
  .||. "_seeds/shorts/*.md"
  .||. "_seeds/shorts/*.txt"

templatesRule =
  "templates/*.html"
  .||. "_seeds/partials/*.html"
