"""
Load the spaces starlark SDK and packages repositories.
"""

if not workspace.is_env_var_set("SPACES_PRINTER_SKIP_SDK_CHECKOUT"):
    checkout.add_repo(
        rule = {"name": "@star/sdk"},
        repo = {
            "url": "https://github.com/work-spaces/sdk",
            "rev": "v0.3.22",
            "checkout": "Revision",
            "clone": "Default",
        },
    )

    checkout.add_repo(
        rule = {"name": "@star/packages"},
        repo = {
            "url": "https://github.com/work-spaces/packages",
            "rev": "v0.2.34",
            "checkout": "Revision",
            "clone": "Default",
        },
    )
