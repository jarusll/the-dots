all: clean link

link:
	stow -d . -t ~/ .

clean:
	stow -d . -t ~/ -D .

update:
	git stash
	git pull
	git stash pop
	git commit -am "Update from ${HOSTNAME}"
