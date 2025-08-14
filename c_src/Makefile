# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s init stop)

PROFILE ?= default
C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT_DIR = $(BASEDIR)/priv/$(PROFILE)
C_SRC_OUTPUT = $(addprefix $(C_SRC_OUTPUT_DIR)/, lz4_nif.so lz4frame_nif.so)
# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -std=c99 -finline-functions -Wall
endif

ifeq ($(PROFILE), test)
	CFLAGS += -Og -g3 -fno-inline-functions -fno-omit-frame-pointer
else
	CFLAGS += -O3 -finline-functions
endif

CFLAGS += -fpic -fPIC

OBJ_CFLAGS = $(CFLAGS)
NIF_CFLAGS = $(CFLAGS) -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -Wmissing-prototypes

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lei
LDFLAGS += -shared

# Verbosity.

VERBOSE ?= 0
ACTUAL_CC := $(CC)
CC_0 = @echo " CC     $@";$(ACTUAL_CC)
CC_1 = $(ACTUAL_CC)
CC = $(CC_$(VERBOSE))

CCLD_0 = @echo " CCLD   $@"; $(ACTUAL_CC)
CCLD_1 = $(ACTUAL_CC)
CCLD = $(CCLD_$(VERBOSE))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -and ! -name "*_nif.c" \))
OBJDIR = $(CURDIR)/$(PROFILE)
OBJECTS = $(addprefix $(OBJDIR)/, $(addsuffix .o, $(basename $(notdir $(SOURCES)))))

.DEFAULT := all
all: $(C_SRC_OUTPUT)

$(C_SRC_OUTPUT_DIR)/%_nif.so: %_nif.c $(OBJECTS)
	@mkdir -p $(C_SRC_OUTPUT_DIR)
	$(CCLD) $< $(OBJECTS) $(NIF_CFLAGS) $(LDFLAGS) $(LDLIBS) -o $@

$(OBJDIR)/%.o: %.c
	@mkdir -p $(OBJDIR)
	$(c_verbose) $(CC) $(OBJ_CFLAGS) -c $< -o $@

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
