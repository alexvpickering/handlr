# handlr

`handlr` makes R functions available as API endpoints. It is the less opinionated cousin of [OpenCPU](https://github.com/opencpu/opencpu).



#### Shared Features with OpenCPU

* Built on Apache2 and rApache
* Each request handled in seperate R fork 
* Borrows code e.g. for parsing http requests



#### Distinguishing Features

* Doesn't expose your R code through GET requests
* You decide what R functions can be run
* Easy user management with [`authr`](https://github.com/alexvpickering/authr)


# Getting Started

Assumes Ubuntu 16.04. See the [wiki](https://github.com/alexvpickering/handlr/wiki) for a more thorough intro and [`authr`](https://github.com/alexvpickering/authr) for user management.

Install Apache2, rApache, and create an `entry.R` file that rApache will run:

```
sudo apt update
sudo apt install apache2 -y

sudo add-apt-repository ppa:opencpu/rapache -y
sudo apt-get update
sudo apt-get install libapache2-mod-r-base -y

sudo mkdir /var/www/R
sudo touch /var/www/R/entry.R
sudo vi /var/www/R/entry.R
```

Type `i` to insert then paste the following:

```R
setHeader(header = "X-Powered-By", value = "rApache")

# functions exported by 'packages' can be used as endpoints
packages <- c('your_package')

handlr::handle(SERVER, GET, packages)
DONE
```
Save (type `esc:wq` then hit enter). Now create an example Apache2 site that will run `entry.R` for incomming http requests:

```
sudo touch /etc/apache2/sites-available/example.conf
sudo vi /etc/apache2/sites-available/example.conf
```

Type `i` to insert, then paste the following:

```apache
LoadModule R_module /usr/lib/apache2/modules/mod_R.so

<Location /api>
	SetHandler r-handler
	RFileHandler /var/www/R/entry.R
</Location>
```

Enable the example site:

```
sudo a2ensite example
sudo service apache2 reload
```

Save and exit as before. Install `handlr` and `your_package`, making sure they will be available to the `www-data` user that Apache2 runs under:

```R
# devtools dependencies
sudo apt install libssl-dev 
sudo apt install libcurl4-openssl-dev

sudo R
.libPaths('/usr/local/lib/R/site-library')

install.packages('sys', configure.vars = 'NO_APPARMOR=1')
install.packages('devtools')

devtools::install_github(c('alexvpickering/handlr', 'your_github_username/your_package'))

```
