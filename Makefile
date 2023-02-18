all: clean link

link:
	stow .

clean:
	stow -D .
