task TaskOne {
  remove "application.debug.old"
  rename "application.debug" "application.debug.old"
  backup "system.error" "system.logs"
  merge "security.logs" "system.logs" "security+system.logs"
}
