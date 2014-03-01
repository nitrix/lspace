#include "window.hpp"

Window::Window() :
    sf::RenderWindow(sf::VideoMode::getFullscreenModes().at(0), "Lonesome Space", sf::Style::Fullscreen)
{
    clear();
    display();
}

Window::~Window()
{
}
