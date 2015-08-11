
-record(safari_alert, {
    title = <<>>    :: apns:apns_str(),
    body = <<>>     :: apns:apns_str(),
    action = <<>>   :: apns:apns_str(),
    url_args = []   :: [apns:apns_str()]
}).