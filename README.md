#Project Description:

User interacts with browser for LCBO product selection given a category, a store, and a required size as inputs.

User registration is as per mechanism of Lift Framework, with whole framework distributed under Apache 2.0 License.

The store selection is by usage of Google MAPS API and user's geolocation being volunteered.
The choice of category is by selection of 6 icons representing each a category, with the set acting as radio buttons.
The choice of required count of product recommendation is a drop down menu offering numbers of 1,5,10, 20, and 50.

## User actions
User actions is via 3 icon buttons beneath the category icons. They are `advise` (question mark image), `cancel` (road no entry sign), and `consume` (a glass
 of wine).

The `advise` action will find matching LCBO products for the given category and store attempting to show as many as requested, one following each other 
vertically.
The recommendation may use caching if a user has previously made requests from the current store and there are products and inventories available
in the database for that store.
Alternatively, the recommendation may make an immediate request to LCBO API from the server to obtain a subset of matching products for reasonable quick 
response, an exercise in partial degradation of service.
In both cases, the advise action randomly selects a match of products. In the cached case, there may be 3000 items in a given store (e.g. wine), so it does a 
random sample algorithm using required size as parameter over cached products of selected category (n choose k problem). In the non-cached case,
we use a random shuffle of the products just obtained (a permutation select one of n! problem) and then restrict to the required count from the shuffle. 
The shuffle or permutation works from the indices of the collection, which is assumed to be a moderately small collection ( a configurable parameter 
as to how many products we are willing to query immediately from the LCBO to satisfy client request, `advisor.maxSampleSize`).

Random algorithms of sampling and shuffling are taken from The Art of Computer Programming by Knuth with minor adjustments to Scala.

The displayed products show an image, attributes of the product, and user input for selection count with dollar value of selection.

The `cancel` action simply erases the products display.

The `consume` action is a simplified simulation of a shopping cart, allowing user to select a count of each product within the proposed list and evaluating a 
total bill.

##Environment and Dependencies
- Run modes: dev and test (`src/main/resources/props:` `default.props` and `test.default.props` whose names are chosen as per Lift framework).
Test mode is used when executing a sample of Scalatest unit test cases.

- Starting app: in project folder, launch sbt, and then `jetty:start`.
May also start from Intellij IDEA Community Edition 2016.*. Patience is required when Intellij IDEA CE decides to index your project.
Runs on localhost:8090 except if launched from IDEA in which case it is localhost:8080 (or the port you specify in run configuration in the IDE).
If running from sbt, the port number is controlled in build.sbt by containerPort,which is tied to plugin xsbt-web-plugin (project/plugins.sbt)

- web server: runs fine on OS X El Capitan 10.11 (initially developed on Yosemite 10.10.5).
browser: runs fine on Google Chrome 50.0. Should run fine on Safari and Firefox.

- Logging: `ch.qos.logback`

- SBT: 0.13.11

- Scala: 2.11.8

- JVM SDK: 1.8

- Jetty

- Cats for State Monad and Xor error handling

- Skinny for WEBAPI client

- Lift framework: 2.6.2 but taken care by SBT
Squeryl, a Scala ORM ((taken care by SBT)
skinny for httpclient (taken care by SBT)
scalatest for unit testing

- Install PostgresSQL. I use 9.4.5.0

## WEB API Dependencies (keys, rate usage)
See `https://lcboapi.com/docs/v1` about setting a LCBO token, which is required if used from a public website as queries are limited
and an important reason why I didn't contemplate deploying to public cloud.

My home private usage qualifies for following:
When LCBO API is accessed from a web server or private script, these keys are not rate-limited, and they do not support CORS or JSONP.

You need a Google MAPS API key as per following (JS API): https://developers.google.com/maps/documentation/javascript/get-api-key
Once obtained, use it in `webapp/index.html` at following replacing `GET_A_KEY` with your API key.

        <script async defer src="https://maps.googleapis.com/maps/api/js?key=GET_A_KEY&callback=storeFinder.initMap"

Install initial SQL tables using `POSTGRES_SCHEMA_INIT.SQL`. What may cause portability issues for other databases in that file are:
Usage of sequences to generate IDs.
Usage of `SERIAL` in users table for primary key (auto increment in other RDBMSs).

## Design Limitations
- Security:
User is expected to accept to provide geo-location, otherwise nothing will work since the closest store is where everything starts while users may select other stores of choice.
There is no login security planned in database and none on web side of things with registration of users not enforcing any password complexity.

- no HA

- no Mobile

##Commentary about design patterns
Behavioral Design Patterns:
template method design pattern (used for loading up caches generically upon a database store)

Functional Design Patterns:
State monad from Cats (for `RNG`, `State` in `RNG.scala`).

Scala-specific design patterns:
enrich my library (`RetainSingles`), similar to C# extension methods
cake pattern (`ProductAdvisor`)
value object (case classes, ubiquitous in Scala)
curiously recurring template pattern (`CRTP`) as found in `Persistable` ( `self: T =>`), tied with Lift Record's design
lazy evaluation (`Store` to inhibit immediate loading of stateful many2many relation `storeProducts`)
Dependency Injection via Partially applied Functions (`LCBOPageFetcherComponent` using partially applied functions for `JSitemsExtractor` and `GotEnough_?` to apply different mechanisms of JSON parsing  )
implicit injection (for type conversion also known as implicit view in Scala in Depth: `P_KEY` and `LCBO_ID` implicitly convert to `Long`, `Store` to XML Node conversion)

Functional Programming: random number generation is a classical example of side effect and does not lend naturally to repeatable unit testing or pure functions implementation.
However, the book Functional Programming in Scala guides developers in addressing this, in particular Chapter 6 for random number generation. This is the approach taken here.
Via configuration, we can let the random number generation be truly random (the seed is randomly controlled) or controlled for unit testing purpose (the seed is chosen by the unit test).

## What it looks like
![alt text][logo]

[logo]: https://github.com/phderome/scalaLiftDemo/blob/master/src/main/webapp/images/AppViewer.png "Landing"

![alt text][logo]

[logo]: https://github.com/phderome/scalaLiftDemo/blob/master/src/main/webapp/images/Selecting.png "Selecting"

![alt text][logo]

[logo]: https://github.com/phderome/scalaLiftDemo/blob/master/src/main/webapp/images/Feedback.png "Feedback"
