
dberl
=====

__Authors:__ Carlos Andres Bolaños R.A. ([`candres@niagara.io`](mailto:candres@niagara.io)).

NoSQL DB access tool written in Erlang.

**DBERL** is also influenced by [SumoDB](https://github.com/inaka/sumo_db), but focused on
NoSQL databases (specially K/V, Document-Oriented and Column-Family).


About
-----

**DBERL** is a framework/tool to make interaction with NoSQL databases from Erlang apps simpler and easier.


Building dberl
--------------

Assuming you have a working Erlang (17 or later) installation, building `dberl` should be as simple as:

    $ git clone https://github.com/cabol/dberl.git
    $ cd dberl
    $ make


DB Providers
------------

Currently only [Couchbase](http://www.couchbase.com) provider is supported.

> **Coming soon**: MongoDB, Cassandra, Riak.


Examples
--------

See [examples](./examples).


Running Tests
-------------

    $ make tests
