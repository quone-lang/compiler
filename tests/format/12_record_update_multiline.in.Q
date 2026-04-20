config <-
    { width = 1280
    , height = 720
    , frame_rate = 30
    , color_depth = 24
    , anti_aliasing = 4
    }

scaled <- { config | width = 1920, height = 1080, frame_rate = 60, color_depth = 32, anti_aliasing = 8 }
