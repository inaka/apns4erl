# Change Log

## [2.2.1](https://github.com/inaka/apns4erl/tree/2.2.1) (2018-07-04)
[Full Changelog](https://github.com/inaka/apns4erl/compare/2.2.0...2.2.1)

**Closed issues:**

- Pseudo-header field ':method' found after regular header [\#211](https://github.com/inaka/apns4erl/issues/211)
- Memory leaks may exist [\#206](https://github.com/inaka/apns4erl/issues/206)
- chatterbox OPT20 gen\_fsm is deprecated and will be removed in a future release [\#184](https://github.com/inaka/apns4erl/issues/184)

**Merged pull requests:**

- send mandatory headers at top [\#212](https://github.com/inaka/apns4erl/pull/212) ([lazedo](https://github.com/lazedo))
- Update from @andreabenini [\#210](https://github.com/inaka/apns4erl/pull/210) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Update README.md [\#204](https://github.com/inaka/apns4erl/pull/204) ([igaray](https://github.com/igaray))
- add erlang 20 for test [\#200](https://github.com/inaka/apns4erl/pull/200) ([getong](https://github.com/getong))

## [2.2.0](https://github.com/inaka/apns4erl/tree/2.2.0) (2017-07-12)
[Full Changelog](https://github.com/inaka/apns4erl/compare/2.1.1...2.2.0)

**Closed issues:**

- Version Bump to 2.2.0 [\#196](https://github.com/inaka/apns4erl/issues/196)
- Add Travis [\#193](https://github.com/inaka/apns4erl/issues/193)
- Write an example with a pool of connections [\#191](https://github.com/inaka/apns4erl/issues/191)
- restrict calls to push\_notification and push\_notification\_token [\#188](https://github.com/inaka/apns4erl/issues/188)
- gen\_server push\_notification return {Timeout, StreamId} [\#182](https://github.com/inaka/apns4erl/issues/182)
- Base64 newline characters [\#180](https://github.com/inaka/apns4erl/issues/180)
- Full Library Revamp [\#84](https://github.com/inaka/apns4erl/issues/84)

**Merged pull requests:**

- \[\#196\] Version Bump to 2.2.0 [\#197](https://github.com/inaka/apns4erl/pull/197) ([ferigis](https://github.com/ferigis))
- Allow binary cert/key in apns\_connection like v1 [\#195](https://github.com/inaka/apns4erl/pull/195) ([danielfinke](https://github.com/danielfinke))
- \[\#193\] Setting Travis up [\#194](https://github.com/inaka/apns4erl/pull/194) ([ferigis](https://github.com/ferigis))
- \[\#191\] Example with a pool of connections [\#192](https://github.com/inaka/apns4erl/pull/192) ([ferigis](https://github.com/ferigis))
- Fix: remove occasional newline characters in base64 output [\#190](https://github.com/inaka/apns4erl/pull/190) ([dgtony](https://github.com/dgtony))
- \[\#188\] restricting push only for connection's owner process [\#189](https://github.com/inaka/apns4erl/pull/189) ([ferigis](https://github.com/ferigis))

## [2.1.1](https://github.com/inaka/apns4erl/tree/2.1.1) (2017-06-22)
[Full Changelog](https://github.com/inaka/apns4erl/compare/2.1.0...2.1.1)

**Closed issues:**

- Version Bump to 2.1.1 [\#186](https://github.com/inaka/apns4erl/issues/186)
- Create connections without name [\#183](https://github.com/inaka/apns4erl/issues/183)

**Merged pull requests:**

- \[\#186\] Version Bump to 2.1.1 [\#187](https://github.com/inaka/apns4erl/pull/187) ([ferigis](https://github.com/ferigis))
- \[\#183\] create connections without name [\#185](https://github.com/inaka/apns4erl/pull/185) ([ferigis](https://github.com/ferigis))
- fixing hex package [\#179](https://github.com/inaka/apns4erl/pull/179) ([ferigis](https://github.com/ferigis))

## [2.1.0](https://github.com/inaka/apns4erl/tree/2.1.0) (2017-05-29)
[Full Changelog](https://github.com/inaka/apns4erl/compare/2.0.0...2.1.0)

**Closed issues:**

- V2 Need async\_push\_notification [\#172](https://github.com/inaka/apns4erl/issues/172)
- apns:push\_notification timeout [\#170](https://github.com/inaka/apns4erl/issues/170)
- Erlang Crashing [\#168](https://github.com/inaka/apns4erl/issues/168)
- where do I register feedback handlers in v2? [\#166](https://github.com/inaka/apns4erl/issues/166)
- Syntax error rebar3 compile [\#165](https://github.com/inaka/apns4erl/issues/165)
- Erlang/OTP 18? [\#164](https://github.com/inaka/apns4erl/issues/164)
- How to find out the invalid token? [\#163](https://github.com/inaka/apns4erl/issues/163)
- update katana dependency [\#153](https://github.com/inaka/apns4erl/issues/153)
- rebar3 compile error [\#152](https://github.com/inaka/apns4erl/issues/152)
- Does  this project  support   apns ‘s HTTP/2 connection ? [\#122](https://github.com/inaka/apns4erl/issues/122)
- Version Bump to 2.1.0 [\#177](https://github.com/inaka/apns4erl/issues/177)
- research in HTTP2 clients [\#175](https://github.com/inaka/apns4erl/issues/175)
- implement apns:get\_feedback/1 [\#167](https://github.com/inaka/apns4erl/issues/167)
- Improve `apns\_utils:sing/1` in order to use `os:cmd/1` instead of `ktn\_os:command/1` [\#159](https://github.com/inaka/apns4erl/issues/159)
- add apns:connection/1 documentation to README [\#156](https://github.com/inaka/apns4erl/issues/156)
- upload to hex [\#151](https://github.com/inaka/apns4erl/issues/151)

**Merged pull requests:**

- \[\#177\] version bump to 2.1.0 [\#178](https://github.com/inaka/apns4erl/pull/178) ([ferigis](https://github.com/ferigis))
- \[\#175\] replace gun by chatterbox [\#176](https://github.com/inaka/apns4erl/pull/176) ([ferigis](https://github.com/ferigis))
- \[\#167\] adding apns:get\_feedback/1 [\#174](https://github.com/inaka/apns4erl/pull/174) ([ferigis](https://github.com/ferigis))
- \[\#156\] update README file [\#173](https://github.com/inaka/apns4erl/pull/173) ([ferigis](https://github.com/ferigis))
- handle timeout config for apns:connect/1 [\#171](https://github.com/inaka/apns4erl/pull/171) ([dcy](https://github.com/dcy))
- Is it possible to use apns4erl and cowboy 1 in the same release? [\#162](https://github.com/inaka/apns4erl/pull/162) ([yzh44yzh](https://github.com/yzh44yzh))
- \[\#159\] – Improve `apns\_utils:sing/1` in order to use `os:cmd/1`. [\#160](https://github.com/inaka/apns4erl/pull/160) ([cabol](https://github.com/cabol))
- export apns:connect/1 [\#155](https://github.com/inaka/apns4erl/pull/155) ([dcy](https://github.com/dcy))
- \[\#152\] update katana dependency [\#154](https://github.com/inaka/apns4erl/pull/154) ([Euen](https://github.com/Euen))

## [2.0.0](https://github.com/inaka/apns4erl/tree/2.0.0) (2017-02-13)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.6-final...2.0.0)

**Closed issues:**

- repeatedly  Reconnecting the Feedback server... [\#121](https://github.com/inaka/apns4erl/issues/121)
- Cannot send notifications  [\#117](https://github.com/inaka/apns4erl/issues/117)
- how can i know which token is invalid? [\#116](https://github.com/inaka/apns4erl/issues/116)
- deps jiffy master support rebar3 [\#115](https://github.com/inaka/apns4erl/issues/115)
- Why don't use a pool or multiple processes to send\_message? [\#109](https://github.com/inaka/apns4erl/issues/109)
- {error, closed} in apns:connect and general cluelessness on my behalf [\#103](https://github.com/inaka/apns4erl/issues/103)
-  How can I get apns4erl integrated with ejabberd  [\#100](https://github.com/inaka/apns4erl/issues/100)
- loss of network connectivity crashes the app [\#80](https://github.com/inaka/apns4erl/issues/80)
- Can't use multiple certificates [\#70](https://github.com/inaka/apns4erl/issues/70)
- APNS return invalid\_token, but the token is ok! [\#66](https://github.com/inaka/apns4erl/issues/66)
- Is there known issues with Erlang 18 ? [\#60](https://github.com/inaka/apns4erl/issues/60)
- Build error on R16B03-01 and R15B03 [\#59](https://github.com/inaka/apns4erl/issues/59)
- no function clause matching ssl\_cipher:hash\_algorithm\(239\) [\#57](https://github.com/inaka/apns4erl/issues/57)
- Version Bump to 1.0.5 [\#55](https://github.com/inaka/apns4erl/issues/55)
- Support latest APNs notification format [\#30](https://github.com/inaka/apns4erl/issues/30)
- test coverage [\#148](https://github.com/inaka/apns4erl/issues/148)
- Update README and rebar in order to support only R19+ [\#146](https://github.com/inaka/apns4erl/issues/146)
- version bump to 2.0.0 [\#145](https://github.com/inaka/apns4erl/issues/145)
- Handling responses improvement [\#138](https://github.com/inaka/apns4erl/issues/138)
- Update Documentation [\#136](https://github.com/inaka/apns4erl/issues/136)
- Push Notifications with Provider's Certificate [\#135](https://github.com/inaka/apns4erl/issues/135)
- close connection gracefully [\#133](https://github.com/inaka/apns4erl/issues/133)
- Get Feedback from APNs [\#132](https://github.com/inaka/apns4erl/issues/132)
- Exponential Backoff when restarting gun connection [\#131](https://github.com/inaka/apns4erl/issues/131)
- apns\_connection should monitor the gun\_connection [\#129](https://github.com/inaka/apns4erl/issues/129)
- Create a Queue for tasks [\#127](https://github.com/inaka/apns4erl/issues/127)
- Create Connections using Provider Auth Token [\#126](https://github.com/inaka/apns4erl/issues/126)
- Create Connections using Provider Certificate [\#125](https://github.com/inaka/apns4erl/issues/125)

**Merged pull requests:**

- \[\#126\] Push notifications with Authentication Token [\#140](https://github.com/inaka/apns4erl/pull/140) ([ferigis](https://github.com/ferigis))
- \[\#133\] create a method for closing the apns connection [\#134](https://github.com/inaka/apns4erl/pull/134) ([ferigis](https://github.com/ferigis))
- app skeleton for version 2 [\#123](https://github.com/inaka/apns4erl/pull/123) ([ferigis](https://github.com/ferigis))
- \[\#145\] Version Bump to 2.0.0 [\#150](https://github.com/inaka/apns4erl/pull/150) ([ferigis](https://github.com/ferigis))
- \[\#148\] test coverage to 100% [\#149](https://github.com/inaka/apns4erl/pull/149) ([ferigis](https://github.com/ferigis))
- \[\#146\] only R19+ supported [\#147](https://github.com/inaka/apns4erl/pull/147) ([ferigis](https://github.com/ferigis))
- \[\#131\] backoff strategy implemented [\#144](https://github.com/inaka/apns4erl/pull/144) ([ferigis](https://github.com/ferigis))
- Ferigis.132.implement feedback [\#142](https://github.com/inaka/apns4erl/pull/142) ([ferigis](https://github.com/ferigis))
- \[\#136\] README updated [\#141](https://github.com/inaka/apns4erl/pull/141) ([ferigis](https://github.com/ferigis))
- Important refactorization [\#139](https://github.com/inaka/apns4erl/pull/139) ([ferigis](https://github.com/ferigis))
- \[\#135\] push notifications with provider certificate [\#137](https://github.com/inaka/apns4erl/pull/137) ([ferigis](https://github.com/ferigis))
- \[\#129\] monitor gun connection [\#130](https://github.com/inaka/apns4erl/pull/130) ([ferigis](https://github.com/ferigis))
- \[\#125\] Connecting to APNs with Provider Certificates [\#128](https://github.com/inaka/apns4erl/pull/128) ([ferigis](https://github.com/ferigis))

## [1.0.6-final](https://github.com/inaka/apns4erl/tree/1.0.6-final) (2016-10-25)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.6...1.0.6-final)

**Closed issues:**

- ssl\_closed after some messages [\#112](https://github.com/inaka/apns4erl/issues/112)
- Can i use one apns4erl to support two client apps? [\#111](https://github.com/inaka/apns4erl/issues/111)
- `apns\_queue` module question [\#108](https://github.com/inaka/apns4erl/issues/108)
- rustyio/sync in rebar.config why? [\#106](https://github.com/inaka/apns4erl/issues/106)
- Update repo and make it ready for hex.pm [\#102](https://github.com/inaka/apns4erl/issues/102)
- Hex Package [\#88](https://github.com/inaka/apns4erl/issues/88)

**Merged pull requests:**

- Allow compilation with erlang 19 [\#114](https://github.com/inaka/apns4erl/pull/114) ([howleysv](https://github.com/howleysv))
- removed extra deps [\#107](https://github.com/inaka/apns4erl/pull/107) ([cystbear](https://github.com/cystbear))
- Invalid return in apns\_connection:handle\_cast [\#104](https://github.com/inaka/apns4erl/pull/104) ([gomoripeti](https://github.com/gomoripeti))
- Add jiffy to the list of applications [\#99](https://github.com/inaka/apns4erl/pull/99) ([unbalancedparentheses](https://github.com/unbalancedparentheses))
- Update link to .pem generation script [\#98](https://github.com/inaka/apns4erl/pull/98) ([unbalancedparentheses](https://github.com/unbalancedparentheses))
- device\_token accept binary and list；fix some elvis errors [\#96](https://github.com/inaka/apns4erl/pull/96) ([zhongwencool](https://github.com/zhongwencool))
- support apns:connect\(ErrorFun, FeedbackFun\) [\#94](https://github.com/inaka/apns4erl/pull/94) ([zhongwencool](https://github.com/zhongwencool))
- apns\_queue.erl has problem [\#91](https://github.com/inaka/apns4erl/pull/91) ([jianjyan](https://github.com/jianjyan))

## [1.0.6](https://github.com/inaka/apns4erl/tree/1.0.6) (2015-11-25)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.5...1.0.6)

**Fixed bugs:**

- Compile error using Erlang R16B02 \(erts-5.10.3\) [\#85](https://github.com/inaka/apns4erl/pull/85) ([jianjyan](https://github.com/jianjyan))

**Closed issues:**

- One of dependencies \(inaka/sync\) now unavailable [\#89](https://github.com/inaka/apns4erl/issues/89)
- jiffy 13.3 has compilation fails with gcc 5.2 [\#86](https://github.com/inaka/apns4erl/issues/86)
- ssl:connect return {error,closed} [\#82](https://github.com/inaka/apns4erl/issues/82)
- Protocol versions are only for sandbox [\#73](https://github.com/inaka/apns4erl/issues/73)
- Add a working certificate so that tests can run [\#71](https://github.com/inaka/apns4erl/issues/71)
- Test Script doesnt work [\#68](https://github.com/inaka/apns4erl/issues/68)
- Bump Version to 1.0.6 [\#63](https://github.com/inaka/apns4erl/issues/63)

**Merged pull requests:**

- Fix sync to use rustyio/sync instead of non-existing inaka/sync [\#90](https://github.com/inaka/apns4erl/pull/90) ([JCzarniecki](https://github.com/JCzarniecki))
- Fix compile error using gcc 5.2 [\#87](https://github.com/inaka/apns4erl/pull/87) ([chenyun90323](https://github.com/chenyun90323))
- Added comma [\#83](https://github.com/inaka/apns4erl/pull/83) ([unbalancedparentheses](https://github.com/unbalancedparentheses))
- Add mockapn [\#78](https://github.com/inaka/apns4erl/pull/78) ([tomekowal](https://github.com/tomekowal))
- Add stack trace on error in feedback function [\#76](https://github.com/inaka/apns4erl/pull/76) ([tomekowal](https://github.com/tomekowal))
- Log clarification [\#75](https://github.com/inaka/apns4erl/pull/75) ([mdaguete](https://github.com/mdaguete))
- Expose extra configuration options for SSL connection [\#74](https://github.com/inaka/apns4erl/pull/74) ([arjan](https://github.com/arjan))
- Only support tls version 1.1 [\#72](https://github.com/inaka/apns4erl/pull/72) ([jeregrine](https://github.com/jeregrine))
- fix erlang now\(\) warning [\#69](https://github.com/inaka/apns4erl/pull/69) ([comtihon](https://github.com/comtihon))
- Version Bump 1.0.6 [\#64](https://github.com/inaka/apns4erl/pull/64) ([davecaos](https://github.com/davecaos))
- Support {M,F} callbacks as well as funs for feedback/error\_fun. [\#62](https://github.com/inaka/apns4erl/pull/62) ([jwheare](https://github.com/jwheare))
- Use tag atom for deps [\#61](https://github.com/inaka/apns4erl/pull/61) ([jwheare](https://github.com/jwheare))
- Updated license [\#58](https://github.com/inaka/apns4erl/pull/58) ([spike886](https://github.com/spike886))

## [1.0.5](https://github.com/inaka/apns4erl/tree/1.0.5) (2015-06-01)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.4...1.0.5)

**Closed issues:**

- SSL: Socket error: etimedout [\#52](https://github.com/inaka/apns4erl/issues/52)
- gen\_server fails with bad return value [\#51](https://github.com/inaka/apns4erl/issues/51)
- delete subscription callback not getting called [\#49](https://github.com/inaka/apns4erl/issues/49)
- How to get query and get data from FeebackService [\#48](https://github.com/inaka/apns4erl/issues/48)
- Handling 'DOWN' connection when sending message using send\_message [\#47](https://github.com/inaka/apns4erl/issues/47)
- How to get .pem file for Apple APNS [\#46](https://github.com/inaka/apns4erl/issues/46)
- How can I get apns4erl integrated with ejabberd [\#45](https://github.com/inaka/apns4erl/issues/45)
- Why Feedback server disconnected as soon as I connected the feedback ok? [\#24](https://github.com/inaka/apns4erl/issues/24)

**Merged pull requests:**

- \[\#55\] 1.0.5 Version Bump [\#56](https://github.com/inaka/apns4erl/pull/56) ([davecaos](https://github.com/davecaos))
- Reset connection on inactivity period [\#54](https://github.com/inaka/apns4erl/pull/54) ([mdaguete](https://github.com/mdaguete))
- Added support for 'category' field [\#53](https://github.com/inaka/apns4erl/pull/53) ([varnit](https://github.com/varnit))
- Update LICENSE [\#50](https://github.com/inaka/apns4erl/pull/50) ([andresinaka](https://github.com/andresinaka))

## [1.0.4](https://github.com/inaka/apns4erl/tree/1.0.4) (2015-02-20)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.3...1.0.4)

**Closed issues:**

- Increase APNS max payload size up to 2 kilobytes [\#42](https://github.com/inaka/apns4erl/issues/42)
- When APNS send an error [\#17](https://github.com/inaka/apns4erl/issues/17)

**Merged pull requests:**

- Increase APNS max payload up to 2 kylobytes [\#44](https://github.com/inaka/apns4erl/pull/44) ([alexdruzhilov](https://github.com/alexdruzhilov))
- update elvis version 0.2.3 to 0.2.4 [\#41](https://github.com/inaka/apns4erl/pull/41) ([cclam0827](https://github.com/cclam0827))
- Rename queue type to queue:queue\(\) [\#40](https://github.com/inaka/apns4erl/pull/40) ([essen](https://github.com/essen))

## [1.0.3](https://github.com/inaka/apns4erl/tree/1.0.3) (2015-01-05)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.2...1.0.3)

**Merged pull requests:**

- APNS resend queue [\#39](https://github.com/inaka/apns4erl/pull/39) ([alexdruzhilov](https://github.com/alexdruzhilov))
- Move to Frame based packets, support message priority [\#38](https://github.com/inaka/apns4erl/pull/38) ([jebu](https://github.com/jebu))

## [1.0.2](https://github.com/inaka/apns4erl/tree/1.0.2) (2014-12-01)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.1...1.0.2)

**Closed issues:**

- test\_certs script doesn't generate working certificate [\#33](https://github.com/inaka/apns4erl/issues/33)
- ssl:connect with the content of the certificate not pem file [\#26](https://github.com/inaka/apns4erl/issues/26)
- Compile error on 17.0 due to erlang\_otp\_vsn [\#19](https://github.com/inaka/apns4erl/issues/19)
- Post 1.0.0 Tagged Release [\#18](https://github.com/inaka/apns4erl/issues/18)
- Unicode Issues [\#15](https://github.com/inaka/apns4erl/issues/15)

**Merged pull requests:**

- Better Docs [\#37](https://github.com/inaka/apns4erl/pull/37) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Replace apns\_mochijson with jiffy [\#36](https://github.com/inaka/apns4erl/pull/36) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Rocked by Elvis [\#35](https://github.com/inaka/apns4erl/pull/35) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Support directly passing key and certificate rather than loading from file [\#34](https://github.com/inaka/apns4erl/pull/34) ([pnc](https://github.com/pnc))
- Remove env-section from apns.app.src [\#32](https://github.com/inaka/apns4erl/pull/32) ([bipthelin](https://github.com/bipthelin))
- Changed the contact us section [\#31](https://github.com/inaka/apns4erl/pull/31) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Export apns:default\_connection/0 to allow key file password to be added,... [\#28](https://github.com/inaka/apns4erl/pull/28) ([robinmacharg](https://github.com/robinmacharg))
- Add support for error status 10 \(shutdown\). [\#27](https://github.com/inaka/apns4erl/pull/27) ([DerGuteMoritz](https://github.com/DerGuteMoritz))
- make compatible with erlang.mk [\#25](https://github.com/inaka/apns4erl/pull/25) ([Euen](https://github.com/Euen))
- Fixed a bug in src/apns.erl making use of password protected keyfiles.... [\#23](https://github.com/inaka/apns4erl/pull/23) ([pkathmann88](https://github.com/pkathmann88))
- adding contact info [\#22](https://github.com/inaka/apns4erl/pull/22) ([marcelog](https://github.com/marcelog))
- Update require\_otp\_vsn [\#20](https://github.com/inaka/apns4erl/pull/20) ([yjh0502](https://github.com/yjh0502))

## [1.0.1](https://github.com/inaka/apns4erl/tree/1.0.1) (2014-02-25)
[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.0...1.0.1)

**Closed issues:**

- Handle disconnections from apple internally [\#1](https://github.com/inaka/apns4erl/issues/1)

**Merged pull requests:**

- allow sending message with content available [\#12](https://github.com/inaka/apns4erl/pull/12) ([michihuber](https://github.com/michihuber))
- Add caveat about passing callbacks using local fun references [\#11](https://github.com/inaka/apns4erl/pull/11) ([RJ](https://github.com/RJ))
- Added ability to include extra data payload into the push packet [\#10](https://github.com/inaka/apns4erl/pull/10) ([mhald](https://github.com/mhald))
- Enabled cover in rebar.config, added cover.spec [\#9](https://github.com/inaka/apns4erl/pull/9) ([igaray](https://github.com/igaray))
- Inaki makefilefix [\#8](https://github.com/inaka/apns4erl/pull/8) ([igaray](https://github.com/igaray))
- Removed tests directory from rebar.config edoc options. [\#7](https://github.com/inaka/apns4erl/pull/7) ([igaray](https://github.com/igaray))
- Update for R16B [\#6](https://github.com/inaka/apns4erl/pull/6) ([kato-im](https://github.com/kato-im))
- -specs for binary arguments [\#5](https://github.com/inaka/apns4erl/pull/5) ([IgorKarymov](https://github.com/IgorKarymov))
- Function to calculate remaining bytes in a payload [\#4](https://github.com/inaka/apns4erl/pull/4) ([pnc](https://github.com/pnc))
- closes \#1 [\#3](https://github.com/inaka/apns4erl/pull/3) ([marcelog](https://github.com/marcelog))

## [1.0.0](https://github.com/inaka/apns4erl/tree/1.0.0) (2012-09-18)
**Closed issues:**

- How-to use is missing [\#2](https://github.com/inaka/apns4erl/issues/2)



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*