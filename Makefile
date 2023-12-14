.SILENT:

# Compiler options
SCALAC = scalac
SCALA = scala
SCALAFLAGS =

# List of program names (without the .scala extension)
PROGRAMS = P01 P02 P03 P04 P05 P06 P07
LAST = $(shell echo $(PROGRAMS) | awk '{print $$NF}')

# Targets
all: clean $(PROGRAMS)

# Compile and execute a program by name (given as an argument)
run:
	@echo "\033[32mCompiling and executing:\033[0m \033[1m$(P)\033[0m"
	$(SCALAC) $(SCALAFLAGS) $(P).scala
	$(SCALA) $(P)

# Compile and execute last program
last: $(LAST)

# Compile and execute each program
$(PROGRAMS): %: %.scala
	@echo "\033[32mCompiling and executing:\033[0m \033[1m$<\033[0m"
	$(SCALAC) $(SCALAFLAGS) $<
	$(SCALA) $@

clean:
	rm -f *.class

# PHONY targets (these targets don't represent files)
.PHONY: all clean run last $(PROGRAMS)


