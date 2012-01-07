module Jekyll

  class BirdBlock < Liquid::Block
    def render(context)
      code = super.join
      code.gsub("<span class='line'><span class=\"o\">&gt;</span>", "<span class='line'>")
    end
  end

end

Liquid::Template.register_tag('bird', Jekyll::BirdBlock)

