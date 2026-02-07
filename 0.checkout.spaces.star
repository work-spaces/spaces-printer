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
            "rev": "ecb7b175935b8bb52c082475083d8d34d84ebfc0",
            "checkout": "Revision",
            "clone": "Default",
        },
    )
