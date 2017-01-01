![](images/obsidian.png)

Obsidian
========

**Study for a regtech contract mirror blockchain.**

This is early draft code to demonstrate the Obsidian concept fu.

Obsidian is process-leaning and scalable. A whitelist of validators restricts network access and allows for *proof-of-publication:* consensus over a set of signed, intrinsically ordered hashes. A second chain of hashes runs on every node to make all its broadcasts tamper-proof at the source. Business logic and timestamp layer are decoupled and fanned out into seven different dimensions of state that allow for historic queries and stress tests. Consensus is based on a list of hashes, not data, as the scope of broadcasting is reduced to interested parties, and auditors.


Features
--------

The Obsidian Concept 1 demonstrates these features:  

* α – passive proof–of–publication consensus
    * non-interactive 
    * non-proposing
    * signature broadcast
    * time order–based
    * completeness–based
    * validation–less

* β – local timestamp chains

* ɣ – cascading whitelist

* δ – id generation

* ε – validated receipts

* ζ – seven layers of state
    * reported events
    * external events
    * event reception order
    * contract state cache
    * ordered business state
    * tests
    * analysis

* η – business state rollback

* θ – big rocket aggregation

* ι – parallel processing

* κ – near metal contracts

* λ – oscillating state optimization

* μ – hardened libs

* ν – hot client upgrade


Consensus
---------

The consensus model is for the purpose to give confirmation of publication. There is no double spending prevention but there is need to have signed proof instead – proof of publication - which proof of work never yields. What is affirmed by peer's signatures are blocks containing but hashes of data that is by nature private and thus not broadcasted or verified. There is no proposal and only one vote in the form of the signing and broadcasting of each node's own block proposal, which order hashes in predetermined order and are thus expected to be identical under fault–free and perfect network conditions. Any deviation is solved by majority. The cost is on the nodes whose hashes did not make it in, translating in no or a reduced amount of peer signatures on their publication. The nodes are not actually interested in the block but the signatures. As every node knows the whitelist it can determine which is the block that has a simple majority of votes with pseudo random tie breakers.  The use of the timestamped hashes is to present them, with the original data, to an auditor that is allowed to inspect the data and can be convinced by the timestamped hash that the data was on the books at the claimed time. The blockchain makes independent of the auditors before that stage, or any central authority, to be able to reliably discharge reporting obligation. In essence, sequential blocks serve as state snapshot that allow previous signature collections from previous blocks to be garbage collected, as the new block and its signatures include the previous one. There is no obvious incentive to double vote or spam. But because of the participation restriction the size of a quorum is known and double-voters and repeat posters are blacklisted by means of the whitelist. Important additional compression is reached by a hash tree that arrives at the block hash by hashing the previous block hash together with tip hashes produces separately for each sender from all its hashes entered for the block and the last such per-sender tip hash of the last block. This allows each sender, who is interested only in proofs for his own hashes, to discard everything except: its own entered hashes all the way from the start, the last two blocks, and the signatures for the last block.


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

The demo runs within one EVM using separate actors (object processes) with no shared state, internal message passing for communication and sharing local files. For clarity this concept does not use behaviors, supervision and OTP.

These are the moving relevant elements:

1. demo controller  
    1. start  
    2. start log
    3. start clients  
    4. benchmark contract
    5. start timed event sequence  
    6. query, verify client state  
    7. shut down  

2. whitelist boot sequence  
    1. start of super auditor  
    2. invites auditor  
    3. invites clients  
    4. broadcast list update  
    5. receive list update   

3. broadcast
    1. determine addressees
    2. encrypt
    3. sign
    4. send
    5. record

4. give receipt
    1. receive request
    2. store as record
    3. sign
    4. send back

5. local record chain  
    1. start  
    2. record entry
    3. chain-hash  
    4. broadcast except broadcasts  
    5. hand over  

6. receive external event or receipt
    1. record reception in local chain
    2. record in event list
    3. process  

7. event list  
    1. start state  
    2. add event / record
    3. header and hash  
    4. order detection  
    5. reorg & backtrack point  
    6. hand over  

8. contract creation  
    1. contract fixtures staging  
    2. code file reading  
    3. compilation, loading  
    4. id registration  
    5. third part lib use  
    6. encrypted broadcast  
    7. auditor receipt  
    8. partner confirmation  
    9. re-broadcast if necessary  
   10. id list broadcast  

9. contract confirmation  
    1. contract receiving  
    2. id registry  
    3. contract storage  
    4. confirmation broadcast  

10. event processing  
    1. establish event order  
    2. track state back for reorgs  
    3. run events by contract's logic  
    4. contract logic processing  
    5. cache state  
    6. hash state & record hash  
    7. hash total & record hash  
    8. determine events  
    9. record, hash, broadcast events  

11. audit
    1. identify hash to be audited
    2. run events vs logic as witnessed
    3. compare and record result
    
12. aggregate report
    1. run known events vs witnessed logic

13. comparative report
    1. run aggregate report
    2. request aggregate reports from all
    3. difference analysis

14. stress tests
    1. receive fictional events
    2. run on event list
    3. process contracts
    4. record result
    5. report results

15. order stress test
    1. define fictional events
    2. broadcast parameters
    3. process locally
    4. receive results
    5. analysis 

16. client code uprade

17. contract benchmark

18. demo action sequence
    1. initiate contract
    2. credit event
    3. annulation of credit event
    4. stress test
    5. full aggregation


Copyright
---------
(c) 2016 Henning Diedrich. All rights reserved.
