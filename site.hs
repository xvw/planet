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

    -- Twtxt
    match "_seeds/twtxt/*.txt" $ do
      route (unseedRoute `composeRoutes` setExtension "txt")
      compile copyFileCompiler

    -- JSOO Artifacts
    match "_seeds/*.bc.js" $ do
      route (unseedRoute `composeRoutes` setExtension "js")
      compile copyFileCompiler

    -- Project seeded (from Planet)
    matchMetadata projectsRule isPublished $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/project.html" projectContext
        >>= loadAndApplyTemplate "templates/default.html" projectContext
        >>= relativizeUrls

    -- Long Stories seeded (from Planet)
    matchMetadata longsRule isPublished $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    -- Short Stories seeded (from Planet)
    matchMetadata shortsRule isPublished $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
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

    -- Projects.html
    match "projects.html" $ do
      route idRoute
      compile $ do

        projects <- recentFirst =<< loadAll projectsRule

        let projectsContext =
              listField "projects" projectContext (return projects) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate projectsContext
          >>= loadAndApplyTemplate "templates/default.html" projectsContext
          >>= relativizeUrls


    -- Longs.html
    match "longs.html" $ do
      route idRoute
      compile $ do

        longs <- recentFirst =<< loadAll longsRule

        let longsContext =
              listField "longs" defaultContext (return longs) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate longsContext
          >>= loadAndApplyTemplate "templates/default.html" longsContext
          >>= relativizeUrls

    -- Shorts.html
    match "shorts.html" $ do
      route idRoute
      compile $ do

        shorts <- recentFirst =<< loadAll shortsRule

        let shortsContext =
              listField "shorts" defaultContext (return shorts) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate shortsContext
          >>= loadAndApplyTemplate "templates/default.html" shortsContext
          >>= relativizeUrls 
        
      
    -- Index.html
    match "index.html" $ do
      route idRoute
      compile $ do
        
        projects <- fmap (take 4) . recentFirst =<< loadAll projectsRule
        longs <- fmap (take 4) . recentFirst =<< loadAll longsRule
        shorts <- fmap (take 8) . recentFirst =<< loadAll shortsRule
        
        let indexContext =
              listField "projects" projectContext (return projects) `mappend`
              listField "shorts" defaultContext (return shorts) `mappend`
              listField "longs" defaultContext (return longs) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "templates/default.html" indexContext
          >>= relativizeUrls

    -- Create Atom Feed
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let f = loadAllSnapshots
        let feedCtx = defaultContext `mappend` bodyField "description"
        longs <- fmap (take 15) . recentFirst =<< f longsRule "content"
        renderAtom myFeedConfiguration feedCtx longs



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

-- Filters
isPublished :: Metadata -> Bool
isPublished ctx =
  lookupString "published" ctx == Just "true"

-- RSS

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "xvw - planet"
    , feedDescription = "Tech blog and time tracker of xvw"
    , feedAuthorName  = "Xavier Van de Woestyne"
    , feedAuthorEmail = "xaviervdw@gmail.com"
    , feedRoot        = "https://xvw.github.io"
    }
