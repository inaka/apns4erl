-record(loc_alert, {body    = none  :: none | string(),
                    action  = none  :: none | string(),
                    key     = ""    :: string(),
                    args    = []    :: [string()],
                    image   = none  :: none | string()}).