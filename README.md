<img style="float: right;" src="https://raw.githubusercontent.com/liordon/domsagxo/master/resources/logo.gif">

# Domsaĝo
A public repository for the Domsaĝo interpreter: a scripting language interpreter based on the Esperanto language.
Domsaĝo (or Domsagxo in x-notation Esperanto, for ascii usage) allows you to program your smart home in a simple imperative manner, allowing you to manipulate simple commands and compose convenient routines. You could think of it simply as a scripting language for your smart home or personal assistant.
More on this story as it develops.

##

## Installation Guide
Requirements:
* For the interpreter itself:
  * Python 3
* For the gui demo:
  * Ability to run graphical programs (directly or through X11)

First, its best to create a virtual environment in which it install domsagxo with `python -m venv domsa_venv`.

Then activate the venv via either `.\domsa_venv\Scripts\activate` on windows or `source domsa_venv/bin/activate` on linux. The next step would be to update pip and install directly from here:
```python -m pip install --upgrade pip
python -m pip install --upgrade setuptools wheel
python -m pip install git+https://github.com/liordon/domsagxo.git
```
And that’s it! Domsaĝo is now installed and ready.
To launch the demo gui, just use the following commands in python:
```import domsagxo
domsagxo.eo_demo()
```
