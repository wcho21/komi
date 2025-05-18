# Komi

## Usage

### Installation

This library is published on the GitHub packages.
Follow the steps below to install, as described in the GitHub packages [documentation][gh-pack].

[gh-pack]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-npm-registry#installing-a-package

Add the following line to your `.npmrc` file:

```ini
//npm.pkg.github.com/:_authToken=<token>
@wcho21:registry=https://npm.pkg.github.com
```

where `<token>` is a GitHub access token (classic) with the `read:packages` permission. 

Install the package with:

```
npm install @wcho21/komi
```