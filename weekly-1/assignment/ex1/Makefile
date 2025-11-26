.PHONY: data clean plot all

all: plot

plot: test_process.pdf test_process_idx.pdf test_process_idx_comm.pdf

test_process.pdf: benchmark
	python plot.py ex1 test_process $(SIZES)

test_process_idx.pdf: benchmark
	python plot.py ex1 test_process_idx $(SIZES)

test_process_idx_comm.pdf: benchmark
	python plot.py ex1 test_process_idx_comm $(SIZES)

benchmark: ex1-opencl.json ex1-c.json ex1-multicore.json

SIZES = 100 1000 10000 100000 1000000 10000000

ex1-opencl.json: ex1.fut
	futhark bench --backend=opencl \
	              --json ex1-opencl.json \
	              ex1.fut

ex1-multicore.json: ex1.fut
	futhark bench --backend=multicore \
	              --json ex1-multicore.json \
	              ex1.fut
				  
ex1-c.json: ex1.fut
	futhark bench --backend=c \
	              --json ex1-c.json \
	              ex1.fut

clean:
	rm -fr data ex1 *.pdf *.bin *.c *.pyc *.json
