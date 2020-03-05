
PWD := $(shell pwd)
PYG := $(PWD)/pygments
PYGEXE := $(PYG)/inst/bin/pygmentize
export PATH := $(PWD)/inst/bin:$(PATH)

PYTHON_VERSION := $(shell python --version | cut -f2 -d' ' | cut -f1,2 -d'.')
export PYTHONPATH := $(PWD)/inst/lib/python$(PYTHON_VERSION)/site-packages

all: $(PYGEXE)

clean:
	rm -rf $(PYG)/{build,dist,inst}

$(PYG):
	git clone --branch 2.5.0 https://github.com/pygments/pygments.git

$(PYG)/pygments/lexers/floorplan.py: floorplan.py $(PYG)
	cp $< $@

$(PYGEXE): $(PYG)/pygments/lexers/floorplan.py
	mkdir -p inst
	cd $(PYG) && make mapfiles
	mkdir -p $(PYTHONPATH)
	cd $(PYG) && python setup.py install --prefix=$(PWD)/inst

