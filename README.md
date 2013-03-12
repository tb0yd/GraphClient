GraphClient
===========

Musings about using any data base as a graph database through a graph client.  See the wiki pages for thoughts and API discussion

setting up the project with rebar.

    dean@dean2:~$ mkdir src
    dean@dean2:~$ cd src
    dean@dean2:~/src$ git clone git://github.com/basho/rebar.git
    Cloning into 'rebar'...
    remote: Counting objects: 5526, done.
    remote: Compressing objects: 100% (2385/2385), done.
    remote: Total 5526 (delta 3553), reused 4819 (delta 2933)
    Receiving objects: 100% (5526/5526), 1.04 MiB, done.
    Resolving deltas: 100% (3553/3553), done.
    dean@dean2:~/src$ cd rebar
    dean@dean2:~/src/rebar$ ./bootstrap

many more lines ...

    Recompile: src/rebar_xref
    ==> rebar (compile)
    ==> rebar (escriptize)
    Congratulations! You now have a self-contained script called "rebar" in
    your current working directory. Place this script anywhere in your path
    and you can use rebar to build OTP-compliant apps.


    dean@dean2:~/src/rebar$ cd ..
    dean@dean2:~/src$ git clone https://github.com/dwagner4/GraphClient.git graph_project
    Cloning into 'graph_project'...
    remote: Counting objects: 28, done.
    remote: Compressing objects: 100% (22/22), done.
    remote: Total 28 (delta 4), reused 22 (delta 3)
    Unpacking objects: 100% (28/28), done.
    dean@dean2:~/src$ cd graph_project
    dean@dean2:~/src/graph_project$ cp ../rebar/rebar .
    dean@dean2:~/src/graph_project$ ./rebar compile
    ==> graph_client (compile)
    ==> graph_project (compile)
    dean@dean2:~/src/graph_project$ tree -L 4
    .
    ├── data1.dat
    ├── lib
    │   └── graph_client
    │       ├── ebin
    │       │   ├── calc.beam
    │       │   ├── graph_client.app
    │       │   ├── graph_client_app.beam
    │       │   └── graph_client_sup.beam
    │       └── src
    │           ├── calc.erl
    │           ├── calc.erl~
    │           ├── graph_client_app.erl
    │           ├── graph_client.app.src
    │           ├── graph_client_sup.erl
    │           └── graph_client_sup.erl~
    ├── README.md
    ├── rebar
    └── rebar.config

    4 directories, 14 files

Then you can run it.

    dean@dean2:~/src/graph_project$ erl -pa lib/*/ebin
    Erlang R15B03 (erts-5.9.3) [source] [64-bit] [smp:8:8] [async-threads:0] [kernel-poll:false]

    Eshell V5.9.3  (abort with ^G)
    1> application:start(graph_client).
    ok

  



