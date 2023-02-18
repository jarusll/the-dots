all: clean link

link:
	stow -d . -t ~/ .

clean:
	stow -d . -t ~/ -D .
