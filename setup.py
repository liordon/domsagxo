import setuptools

with open("resources/plain-readme.txt", "r") as reamde:
    description = reamde.read()

with open('requirements.txt') as f:
    requirements = f.read().splitlines()

setuptools.setup(
    name="domsagxo",
    version="0.0.1",
    author="Lior Samuel",
    author_email="liorsam@campus.technion.ac.il",
    description="Domsagxo -- an Esperanto-based vocal scripting language for smart home or VUI",
    long_description=description,
    long_description_content_type="text/markdown",
    url="http://github.com/liordon/domsagxo",
    packages=setuptools.find_packages(where=".", exclude=("domsagxo/syntax_high_light", "test")),
    include_package_data=True,
    setup_requires=['wheel'],
    install_requires=requirements,
    classifiers=[
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
    ],
)
