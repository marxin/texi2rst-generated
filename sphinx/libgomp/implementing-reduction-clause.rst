.. _implementing-reduction-clause:

Implementing REDUCTION clause
*****************************

The private struct mentioned in the previous section should have 
a pointer to an array of the type of the variable, indexed by the 
thread's :samp:`{team_id}`.  The thread stores its final value into the 
array, and after the barrier, the master thread iterates over the
array to collect the values.

