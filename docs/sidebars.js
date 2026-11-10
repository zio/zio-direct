const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Direct Style",
      link: { type: "doc", id: "index" },
      collapsed: false,
      items: [
        "tutorial-rewrite-for-comprehension",
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
        },
        {
          type: "category",
          label: "Reference",
          collapsed: false,
          items: [
            "supported-constructs",
            "restrictions-and-error-messages",
            "configuration"
          ]
        }
      ]
    }
  ]
};

module.exports = sidebars;
