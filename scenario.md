Using K/V  on-chain store

* GH users have associated pk (ed25519)

Use cases: Open Source project tracking

* Oracle verifies that requests (facts) match the reality in GH
* can compose several facts independently from differetn tokens (given the root and proof)

## Requests

###  Identify (authorise?) users?

* requester: the user
* key: platform / userid
  * https://github.com/abailly.keys
* value: hash (ed25519.pk)
* oracle:
  * verifies the user has the key they claim they have off-chain
  * associates in the DB the user with a spending key

### Authorise a user for a given repo

* requester: a user (anyone)
* key: platform / repository / "maintainer"
  * in theory we want to support multiple roles but having only maintainer is fine for now
* value: platform / userid
* oracle:
  * verifies the userid is known in DB
  * Verifies the role within the repo

### Trigger Antithesis reviews

* User wants to trigger AT run
* Requests are posted on-chain with references to PR/artefacts
* "Oracle" accepts (or not) the request and outputs test results
* Later on we could lock some payment in smart contract to be released with test results
* key: platform / user
* value:
  * repo URL
  * commit Hash
  * signature
* oracle:
  * verifies PR/commit
  * trigger test run

Q: do we need a "registered" user here?

### Certify a release

* requester: a maintainer of the repo (identified user in the DB)
* key: platform / repo / version
* value:
  * tarball URL
  * tarball Hash
  * signature(tarball hash, maintainer)
  * commit hash
* oracle (all off-chain, some of it could be done on-chain to limit oracle's power):
  * verifies the version ∃ on GH
  * verifies the tarball URL and the hash
  * verifies the signature
  * Verifies the commit hash ∃
* NOTE:
  * enforce uniqueness of release (eg. immutable key/value) which goes against GH policy (eg. a release tarball can be changed)
