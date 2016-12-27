![](images/obsidian.png)

Obsidian
========

**Regtech mirror blockchain.**


Features
--------

Obsidian Concept Demo demonstrates these features:  

* local timestamp chain

* id generation & list

* big rocket aggregation

* passive consensus
    * non-interactive 
    * time order–based
    * completeness–based
    * non-proposing
    * validation–less

* cascading whitelist

* validated receipts

* six layers of state
    * reported events
    * external events
    * event reception order
    * ordered business state
    * tests
    * analysis

* business state rollback

* oscillating state optimization

* three scopes of state
    * internal
    * partnered
    * aggregated

* near metal contracts

* hardened libs

* hot client upgrade


Differentiation
---------------

Obsidian has
* no consensus over calculated state   
* no global, universally accessible state
* no cryptocurrency
* no double–spending protection  
* no anonymous validators
* no anonymous transactors
* no identification via public key

	
Test Setup
----------

3 clients  
1 auditor  
1 super auditor 

The demo runs within one EVM using different processes with no shared state but internal message passing and sharing local files.  

0. demo controller
    1. start
    2. start clients
    3. start timed sequence
    4. query, verify client state
    5. shut down 

1. whitelist boot sequence
    1. start of super auditor  
    2. invites auditor  
    3. invites clients  
    4. broadcast list  

2. local chain log  
    1. start  
    2. add entry  
    3. broadcast hash  
    4. hand over  

3. event list  
    1. start state
    2. local entry  
    3. external entry  
    4. header and hash  
    5. order detection
    6. reorg & backtrack point
    7. hand over  

4. contract registration  
    1. contract fixtures staging  
    2. code file reading  
    3. compilation, loading  
    4. id registration
    5. third part lib use
    6. encrypted broadcast  
    7. auditor receipt  
    8. partner confirmation  
    9. id list broadcast

5. contract confirmation
    1. contract receiving
    2. id registry
    3. contract storage
    4. confirmation broadcast

6. event processing
    1. establish event order
    2. track state back for reorgs
    3. run events by contract's logic
    4. contract logic processing
    5. cache state
    6. hash state & record hash


Copyright
---------
(c) 2016 Henning Diedrich. All rights reserved.
