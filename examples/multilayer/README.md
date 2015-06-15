Multilayer example
==================

This example shows how to build a multilayer app:

 * RESTful Service Layer (using Cowboy).
 * Data Access Layer (using Repository patten with `dberl`, which is the main purpose in this example).
 * Domain Model (only one entity `event`).

So the general idea, is expose a RESTful service to allow create/delete/get events.


Build Multilayer
----------------

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```


Setup DB
--------

Before to start the app, you need to setup the DB.

### Couchbase

You can follow these steps using Couchbase Web Admin Console, CURL or other client.

* Create the bucket `multilayer_events`.

* Create design document `queries`.

* Create within previous created design doc the view `multilayer_events`. This is the
  JS code of the view:

```javascript
function (doc, meta) {
  if (doc.type && doc.type == "event") {
    if (doc.event_type) {
      emit(doc.event_type, doc);
    }
  }
}
```


Start Multilayer
----------------

To start the release in the foreground:

``` bash
$ ./_rel/multilayer/bin/multilayer console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).


Example output
--------------

Create a new event:

``` bash
$ curl -v -X POST -H "Content-Type: application/json" "http://localhost:8080/events?event_type=eventA&comment=hello"
>
< HTTP/1.1 201 Created
< connection: keep-alive
* Server Cowboy is not blacklisted
< server: Cowboy
< date: Mon, 15 Jun 2015 17:33:10 GMT
< content-length: 0
< content-type: application/json
< location: /events/Uci1S4gw6iCOgPZW9Hxwng
<
```

Get created event (the `id` comes in the location header in previous POST):

``` bash
$ curl -v http://localhost:8080/events/Uci1S4gw6iCOgPZW9Hxwng
>
< HTTP/1.1 200 OK
< connection: keep-alive
* Server Cowboy is not blacklisted
< server: Cowboy
< date: Mon, 15 Jun 2015 17:35:11 GMT
< content-length: 175
< content-type: application/json
<
{
 "event_type":"eventA",
 "comment":"hello",
 "created_at":"Mon, 15 Jun 2015 17:33:10 GMT",
  "updated_at":"Mon, 15 Jun 2015 17:33:10 GMT",
 "type":"event","id":"Uci1S4gw6iCOgPZW9Hxwng"
 }
```

Delete created event:

``` bash
$ curl -v -X DELETE http://localhost:8080/events/Uci1S4gw6iCOgPZW9Hxwng
>
< HTTP/1.1 204 No Content
< connection: keep-alive
* Server Cowboy is not blacklisted
< server: Cowboy
< date: Mon, 15 Jun 2015 17:42:17 GMT
< content-length: 0
< content-type: application/json
<
```

Now create more events, so you can test to get all events by `event_type`.

Supposing that you have created N events, and 2 of type `eventA`:

``` bash
$ curl -v "http://localhost:8080/events?event_type=eventA"
>
< HTTP/1.1 200 OK
< connection: keep-alive
* Server Cowboy is not blacklisted
< server: Cowboy
< date: Mon, 15 Jun 2015 18:25:41 GMT
< content-length: 355
< content-type: application/json
<
[
 {
  "updated_at":"Mon, 15 Jun 2015 18:14:14 GMT",
  "type":"event","id":"YhQMY9OYlM7ExvQ6bMzV1g",
  "event_type":"eventA",
  "created_at":"Mon, 15 Jun 2015 18:14:14 GMT",
  "comment":"hello"
 },
 {
  "updated_at":"Mon, 15 Jun 2015 18:14:17 GMT",
  "type":"event","id":"zGY9Ri48We1Pwjn06tSgOw",
  "event_type":"eventA",
  "created_at":"Mon, 15 Jun 2015 18:14:17 GMT",
  "comment":"hello"
 }
]
```
