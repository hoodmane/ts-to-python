#!/bin/bash
mkdir -p generated/js/
npm run -C type-generation run ../input-projects/browser-dom/ ../generated/js/__init__.pyi
(cd input-projects/cloudflare-workers/ && npx wrangler types)
npm run -C type-generation run ../input-projects/cloudflare-workers/ ../generated/js/cloudflare.pyi
