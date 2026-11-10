const sidebars = {
  sidebar: [
    "index",
    "getting-started",
    {
      type: "category",
      label: "Reference",
      collapsed: false,
      items: [
        "supported-constructs",
        "restrictions-and-error-messages",
        "configuration"
      ]
    },
    {
      type: "category",
      label: "How-to Guides",
      collapsed: false,
      items: [
        "how-to-handle-errors",
        "how-to-run-in-parallel",
        "how-to-debug-defer",
        "intellij-support",
        "other-supported-monads"
      ]
    }
  ]
};

module.exports = sidebars;
