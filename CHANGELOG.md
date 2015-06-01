# Change Log

## [1.0.5](https://github.com/inaka/apns4erl/tree/HEAD)

[Full Changelog](https://github.com/inaka/apns4erl/compare/1.0.4...HEAD)

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

- Increase apns payload [\#43](https://github.com/inaka/apns4erl/pull/43) ([alexdruzhilov](https://github.com/alexdruzhilov))

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

- Unmerged changes from addressing comments [\#16](https://github.com/inaka/apns4erl/pull/16) ([hemantkr](https://github.com/hemantkr))

- Contrib [\#14](https://github.com/inaka/apns4erl/pull/14) ([elbrujohalcon](https://github.com/elbrujohalcon))

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

- Reconnect [\#13](https://github.com/inaka/apns4erl/pull/13) ([hemantkr](https://github.com/hemantkr))

## [1.0.0](https://github.com/inaka/apns4erl/tree/1.0.0) (2012-09-18)

**Closed issues:**

- How-to use is missing [\#2](https://github.com/inaka/apns4erl/issues/2)



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*