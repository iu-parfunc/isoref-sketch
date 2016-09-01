
Sketches and mockups of memory-protection concepts
==================================================

There are a few different prototypes in this repo:

 * Iso refs -- dynamically enforced linear pointers.
   The idea is that an `Iso s (Compact a)` would be
   sufficient for deterministic freeing.

  - IsoST03 is the one to look at.
  - IsoVec02 and IsoRef01 were WIP sketches that explore
    other alternatives.

 * MVarLock - an "s" indexed lock that protects a specific piece of ST
   state.  This allows IO-based critical sections where the type
   system enforces that the appropriate lock is held.
 
 * ThreadLocalLock - simply use a runtime TID test to ensure that
   state is only used with one thread.  Much cheaper than an actual
   lock.

 * IndexedMonad/ - this directory is a scratchpad where I (RRN) was
   toying around with one of Oleg's IndexedMonad prototypes.  I was
   experimenting with TemplateHaskell hacks to provide unique naming
   of state references at both the type level and as a lexical
   variable.  (I.e. avoiding the need to write the name more than once.)
