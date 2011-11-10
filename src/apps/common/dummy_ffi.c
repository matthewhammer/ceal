
void trace_a_step(char* file, char* funname, int line) foreign {
  return;
}

void trace_a_step_invoke(void* trnode, void** _, char* file, char* funname, int line) foreign {
  return;
}

void trace_a_step_revoke(void* trnode, void** _) foreign {
  return;
}

void trace_a_step_revinv(void* trnode, void** _) foreign {
  return;
}

#pragma CEAL_ffi(trace_a_step, invoke, revoke, revinv, wakeup, nondet)
