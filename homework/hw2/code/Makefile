ML_FILES := aeson.ml rlist.ml ast.ml state.ml auth.ml lexer.ml parser.ml interp.ml
MLY_FILES := parser.mly
MLL_FILES := lexer.mll
SOURCES := $(ML_FILES) $(MLY_FILES) $(MLL_FILES) server.ml cmdline.ml
OBJS := $(ML_FILES:.ml=.cmx)

COMP := ocamlopt

LIBDIR := -I `ocamlfind query easy-format` -I `ocamlfind query biniou` -I `ocamlfind query yojson`
LIBMOD := easy_format.cmx bi_util.cmx bi_share.cmx bi_outbuf.cmx bi_inbuf.cmx bi_vint.cmx bi_io.cmx yojson_biniou.cmx yojson.cmx

CSLFLAGS = -g

EXE = interp
SERVER = server

all: $(SERVER) $(EXE)

$(EXE): $(OBJS) cmdline.cmx
	$(COMP) $(LIBDIR) -g $(LIBMOD) unix.cmxa $^ -o $(EXE)

$(SERVER): $(OBJS) server.cmx
	$(COMP) $(LIBDIR) -g $(LIBMOD) str.cmxa unix.cmxa -o $(SERVER) $^

########################################
# implicit rules for ocaml compilation #
########################################
%.cmi : %.mli
	$(COMP) $(CSLFLAGS) -c $< -o $@

%.cmi : %.ml
	$(COMP) $(CSLFLAGS) -I `ocamlfind query yojson` -c $< -o $@

%.cmx : %.ml
	$(COMP) $(CSLFLAGS) -I `ocamlfind query yojson` -c $< -o $@

%.ml %.mli : %.mly
	ocamlyacc -v $<

%.ml : %.mll
	ocamllex $<

.dep: $(SOURCES)
	ocamldep *.ml* > .dep

depend: .dep

parser.cmi: parser.mli ast.cmi

-include .dep

clean:
	$(RM) lexer.ml parser.ml parser.mli *.cm* *.annot *.o $(EXE) $(SERVER) $(SERVER0) $(SERVER1) $(SERVER2) parser.output .dep
